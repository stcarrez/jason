-----------------------------------------------------------------------
--  jason-tickets-modules -- Module tickets
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
with AWA.Permissions;
with AWA.Users.Models;
with Util.Log.Loggers;
with Jason.Tickets.Beans;
with ADO.Sessions;
with AWA.Services.Contexts;
package body Jason.Tickets.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Jason.Tickets.Module");

   package Register is new AWA.Modules.Beans (Module => Ticket_Module,
                                              Module_Access => Ticket_Module_Access);

   --  ------------------------------
   --  Initialize the tickets module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Ticket_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the tickets module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Tickets.Beans.Ticket_Bean",
                         Handler => Jason.Tickets.Beans.Create_Ticket_Bean'Access);
      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Tickets.Beans.Ticket_List_Bean",
                         Handler => Jason.Tickets.Beans.Create_Ticket_List_Bean'Access);
      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Tickets.Beans.Ticket_Status_List_Bean",
                         Handler => Jason.Tickets.Beans.Create_Status_List'Access);
      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Tickets.Beans.Ticket_Type_List_Bean",
                         Handler => Jason.Tickets.Beans.Create_Type_List'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the tickets module.
   --  ------------------------------
   function Get_Ticket_Module return Ticket_Module_Access is
      function Get is new AWA.Modules.Get (Ticket_Module, Ticket_Module_Access, NAME);
   begin
      return Get;
   end Get_Ticket_Module;

   --  ------------------------------
   --  Load the ticket.
   --  ------------------------------
   procedure Load_Ticket (Model    : in Ticket_Module;
                          Ticket   : in out Jason.Tickets.Models.Ticket_Ref'Class;
                          Project  : in out Jason.Projects.Models.Project_Ref'Class;
                          Tags     : in out AWA.Tags.Beans.Tag_List_Bean;
                          Id       : in ADO.Identifier) is
      DB    : ADO.Sessions.Session := Model.Get_Session;
      Found : Boolean;
   begin
      Ticket.Load (DB, Id, Found);
      Project := Ticket.Get_Project;
      Tags.Load_Tags (DB, Id);
   end Load_Ticket;

   --  ------------------------------
   --  Create
   --  ------------------------------
   procedure Create (Model      : in Ticket_Module;
                     Entity     : in out Jason.Tickets.Models.Ticket_Ref'Class;
                     Project_Id : in ADO.Identifier) is
      pragma Unreferenced (Model);

      Ctx   : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
      User  : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Project : Jason.Projects.Models.Project_Ref;
   begin
      --  Check that the user has the create ticket permission on the given project.
      AWA.Permissions.Check (Permission => ACL_Create_Tickets.Permission,
                             Entity     => Project_Id);

      Ctx.Start;
      Project.Load (DB, Project_Id);
      Project.Set_Last_Ticket (Project.Get_Last_Ticket + 1);
      Entity.Set_Create_Date (Ada.Calendar.Clock);
      Entity.Set_Status (Jason.Tickets.Models.OPEN);
      Entity.Set_Creator (Ctx.Get_User);
      Entity.Set_Project (Project);
      Entity.Set_Ident (Project.Get_Last_Ticket);
      Entity.Save (DB);
      Project.Save (DB);
      Ctx.Commit;

      Log.Info ("Ticket {0} created for user {1}",
                ADO.Identifier'Image (Entity.Get_Id), ADO.Identifier'Image (User));
   end Create;

   --  ------------------------------
   --  Save
   --  ------------------------------
   procedure Save (Model  : in Ticket_Module;
                   Entity : in out Jason.Tickets.Models.Ticket_Ref'Class) is
      pragma Unreferenced (Model);

      Ctx   : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      DB    : ADO.Sessions.Master_Session := AWA.Services.Contexts.Get_Master_Session (Ctx);
   begin
      --  Check that the user has the update ticket permission on the given ticket.
      AWA.Permissions.Check (Permission => ACL_Update_Tickets.Permission,
                             Entity     => Entity);

      Ctx.Start;
      Entity.Set_Update_Date (Ada.Calendar.Clock);
      Entity.Save (DB);
      Ctx.Commit;
   end Save;

end Jason.Tickets.Modules;
