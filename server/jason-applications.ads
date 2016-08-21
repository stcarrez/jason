-----------------------------------------------------------------------
--  jason -- jason applications
-----------------------------------------------------------------------
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
with ASF.Servlets.Faces;
with ASF.Servlets.Files;
with ASF.Servlets.Ajax;
with ASF.Filters.Dump;
with ASF.Servlets.Measures;

with ASF.Security.Servlets;

with AWA.Users.Servlets;
with AWA.Users.Modules;
with AWA.Mail.Modules;
with AWA.Comments.Modules;
with AWA.Blogs.Modules;
with AWA.Tags.Modules;
with AWA.Storages.Modules;
with AWA.Applications;
with AWA.Workspaces.Modules;
with AWA.Services.Filters;
with Jason.Projects.Modules;
with Jason.Tickets.Modules;
package Jason.Applications is

   CONFIG_PATH  : constant String := "/jason";
   CONTEXT_PATH : constant String := "/jason";

   type Application is new AWA.Applications.Application with private;
   type Application_Access is access all Application'Class;

   --  Initialize the application.
   procedure Initialize (App : in Application_Access);

   --  Initialize the servlets provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application servlets.
   overriding
   procedure Initialize_Servlets (App : in out Application);

   --  Initialize the filters provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the application filters.
   overriding
   procedure Initialize_Filters (App : in out Application);

   --  Initialize the AWA modules provided by the application.
   --  This procedure is called by <b>Initialize</b>.
   --  It should register the modules used by the application.
   overriding
   procedure Initialize_Modules (App : in out Application);

   --  Create the Jason application instance.
   function Create return Application_Access;

private

   type Application is new AWA.Applications.Application with record
      Self              : Application_Access;
      --  Application servlets and filters (add new servlet and filter instances here).
      Faces             : aliased ASF.Servlets.Faces.Faces_Servlet;
      Ajax              : aliased ASF.Servlets.Ajax.Ajax_Servlet;
      Files             : aliased ASF.Servlets.Files.File_Servlet;
      Dump              : aliased ASF.Filters.Dump.Dump_Filter;
      Service_Filter    : aliased AWA.Services.Filters.Service_Filter;
      Measures          : aliased ASF.Servlets.Measures.Measure_Servlet;

      --  Authentication servlet and filter.
      Auth              : aliased ASF.Security.Servlets.Request_Auth_Servlet;
      Verify_Auth       : aliased AWA.Users.Servlets.Verify_Auth_Servlet;

      --  The application modules.
      User_Module       : aliased AWA.Users.Modules.User_Module;
      Workspace_Module  : aliased AWA.Workspaces.Modules.Workspace_Module;
      Blog_Module       : aliased AWA.Blogs.Modules.Blog_Module;
      Mail_Module       : aliased AWA.Mail.Modules.Mail_Module;
      Comment_Module    : aliased AWA.Comments.Modules.Comment_Module;
      Storage_Module    : aliased AWA.Storages.Modules.Storage_Module;
      Tag_Module        : aliased AWA.Tags.Modules.Tag_Module;

      --  Add your modules here.
      Project_Module    : aliased Jason.Projects.Modules.Project_Module;
      Ticket_Module     : aliased Jason.Tickets.Modules.Ticket_Module;
   end record;

end Jason.Applications;
