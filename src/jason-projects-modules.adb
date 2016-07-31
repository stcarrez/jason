-----------------------------------------------------------------------
--  jason-projects-modules -- Module projects
--  Copyright (C) 2016 Stephane.Carrez
--  Written by Stephane.Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Calendar;
with AWA.Modules.Beans;
with AWA.Modules.Get;
with AWA.Permissions.Services;
with Util.Log.Loggers;
with Jason.Projects.Beans;
with ADO.Sessions;
with AWA.Services.Contexts;
package body Jason.Projects.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Jason.Projects.Module");

   package Register is new AWA.Modules.Beans (Module => Project_Module,
                                              Module_Access => Project_Module_Access);

   --  ------------------------------
   --  Initialize the projects module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Project_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the projects module");

      --  Setup the resource bundles.
      App.Register ("projectMsg", "projects");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Projects.Beans.Projects_Bean",
                         Handler => Jason.Projects.Beans.Create_Project_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the projects module.
   --  ------------------------------
   function Get_Project_Module return Project_Module_Access is
      function Get is new AWA.Modules.Get (Project_Module, Project_Module_Access, NAME);
   begin
      return Get;
   end Get_Project_Module;


   --  ------------------------------
   --  Create
   --  ------------------------------
   procedure Create (Model  : in Project_Module;
                     Entity : in out Jason.Projects.Models.Project_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Ctx.Start;
      Entity.Set_Create_Date (Ada.Calendar.Clock);
      Entity.Set_Owner (Ctx.Get_User);
      Entity.Save (DB);

      --  Add the permission for the user to use the new project.
      AWA.Permissions.Services.Add_Permission (Session => DB,
                                               User    => User,
                                               Entity  => Entity);
      Ctx.Commit;
      Log.Info ("Project {0} created for user {1}",
                ADO.Identifier'Image (Entity.Get_Id), ADO.Identifier'Image (User));
   end Create;

   --  ------------------------------
   --  Save
   --  ------------------------------
   procedure Save (Model  : in Project_Module;
                   Entity : in out Jason.Projects.Models.Project_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      Log.Info ("Updating project {0}", ADO.Identifier'Image (Entity.Get_Id));

      --  Check that the user has the update permission on the given project.
      AWA.Permissions.Check (Permission => ACL_Update_Projects.Permission,
                             Entity     => Entity);

      Ctx.Start;
      Entity.Set_Update_Date (Ada.Calendar.Clock);
      Entity.Save (DB);
      Ctx.Commit;
   end Save;

   --  ------------------------------
   --  Load the project information.
   --  ------------------------------
   procedure Load_Project (Model   : in Project_Module;
                           Project : in out Jason.Projects.Models.Project_Ref'Class;
                           Tags    : in out AWA.Tags.Beans.Tag_List_Bean;
                           Id      : in ADO.Identifier) is
      DB    : ADO.Sessions.Session := Model.Get_Session;
      Found : Boolean;
   begin
      --  Check that the user has the view page permission on the given wiki page.
      --  AWA.Permissions.Check (Permission => ACL_View_Wiki_Page.Permission,
      --                       Entity     => Id);

      Project.Load (DB, Id, Found);
      Tags.Load_Tags (DB, Id);
   end Load_Project;

end Jason.Projects.Modules;
