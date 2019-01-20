-----------------------------------------------------------------------
--  jason-projects-modules -- Module projects
--  Copyright (C) 2016, 2017, 2019 Stephane.Carrez
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
with AWA.Permissions;
with AWA.Workspaces.Models;
with AWA.Workspaces.Modules;
with Util.Log.Loggers;
with Jason.Projects.Beans;
with Jason.Tickets.Modules;
with ADO.Sessions;
with ADO.SQL;
with AWA.Services.Contexts;
package body Jason.Projects.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Jason.Projects.Module");

   package ASC renames AWA.Services.Contexts;

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
      Plugin.Percent_Converter.Set_Picture ("ZZ9.9");
      App.Add_Converter (Name      => "percentConverter",
                         Converter => Plugin.Percent_Converter'Unchecked_Access);
      Plugin.Hour_Converter.Set_Picture ("ZZ9");
      App.Add_Converter (Name      => "durationConverter",
                         Converter => Plugin.Hour_Converter'Unchecked_Access);

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Projects.Beans.Projects_Bean",
                         Handler => Jason.Projects.Beans.Create_Project_Bean'Access);

      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Projects.Beans.Project_List_Bean",
                         Handler => Jason.Projects.Beans.Create_Project_List_Bean'Access);

      --  Get the wiki module.
      Plugin.Wiki := AWA.Wikis.Modules.Get_Wiki_Module;

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

      use Jason.Tickets.Modules;
      use AWA.Wikis.Modules;

      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
      WS    : AWA.Workspaces.Models.Workspace_Ref;
   begin
      Ctx.Start;
      AWA.Workspaces.Modules.Get_Workspace (DB, Ctx, WS);

      --  Check that the user has the create permission on the workspace.
      AWA.Permissions.Check (Permission => ACL_Create_Projects.Permission,
                             Entity     => WS);

      Entity.Set_Create_Date (Ada.Calendar.Clock);
      Entity.Set_Owner (Ctx.Get_User);
      Entity.Save (DB);

      --  Add the permission for the user to use the new project.
      AWA.Workspaces.Modules.Add_Permission (Session   => DB,
                                             User      => User,
                                             Entity    => Entity,
                                             Workspace => WS.Get_Id,
                                             List => (ACL_Update_Projects.Permission,
                                                      ACL_Delete_Projects.Permission,
                                                      ACL_Create_Tickets.Permission,
                                                      ACL_Delete_Tickets.Permission,
                                                      ACL_Update_Tickets.Permission,
                                                      ACL_Create_Wiki_Space.Permission));
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

      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
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
   --  Create the project wiki space.
   --  ------------------------------
   procedure Create_Wiki (Model  : in Project_Module;
                          Entity : in out Jason.Projects.Models.Project_Ref'Class;
                          Wiki   : in out AWA.Wikis.Models.Wiki_Space_Ref'Class) is
      Ctx   : constant ASC.Service_Context_Access := ASC.Current;
      DB    : ADO.Sessions.Master_Session := ASC.Get_Master_Session (Ctx);
   begin
      Log.Info ("Creating project wiki space {0}", ADO.Identifier'Image (Entity.Get_Id));

      --  Check that the user has the update permission on the given project.
      AWA.Permissions.Check (Permission => ACL_Update_Projects.Permission,
                             Entity     => Entity);

      Ctx.Start;
      Model.Wiki.Create_Wiki_Space (Wiki);
      Entity.Set_Wiki (Wiki);
      Entity.Set_Update_Date (Ada.Calendar.Clock);
      Entity.Save (DB);
      Ctx.Commit;
   end Create_Wiki;

   --  ------------------------------
   --  Load the project information.
   --  ------------------------------
   procedure Load_Project (Model   : in Project_Module;
                           Project : in out Jason.Projects.Models.Project_Ref'Class;
                           Wiki    : in out AWA.Wikis.Models.Wiki_Space_Ref'Class;
                           Tags    : in out AWA.Tags.Beans.Tag_List_Bean;
                           Id      : in ADO.Identifier;
                           Wiki_Id : in ADO.Identifier) is
      use type ADO.Identifier;

      DB    : ADO.Sessions.Session := Model.Get_Session;
      Found : Boolean;
      Query : ADO.SQL.Query;
   begin
      --  Check that the user has the view page permission on the given wiki page.
      --  AWA.Permissions.Check (Permission => ACL_View_Wiki_Page.Permission,
      --                       Entity     => Id);

      if Id /= ADO.NO_IDENTIFIER then
         Project.Load (DB, Id, Found);
         if Found and then not Project.Get_Wiki.Is_Null then
            Wiki.Load (DB, Project.Get_Wiki.Get_Id, Found);
         end if;
         Tags.Load_Tags (DB, Id);
      else
         Query.Bind_Param (1, Wiki_Id);
         Query.Set_Filter ("o.wiki_id = ?");
         Project.Find (DB, Query, Found);
         if Found then
            Wiki.Load (DB, Wiki_Id);
            Tags.Load_Tags (DB, Project.Get_Id);
         end if;
      end if;
   end Load_Project;

end Jason.Projects.Modules;
