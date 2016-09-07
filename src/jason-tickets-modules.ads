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
with ASF.Applications;
with ADO;
with AWA.Modules;
with AWA.Tags.Beans;
with Jason.Tickets.Models;
with Jason.Projects.Models;
with Security.Permissions;
package Jason.Tickets.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "tickets";

   package ACL_Create_Tickets is new Security.Permissions.Definition ("ticket-create");
   package ACL_Delete_Tickets is new Security.Permissions.Definition ("ticket-delete");
   package ACL_Update_Tickets is new Security.Permissions.Definition ("ticket-update");

   --  ------------------------------
   --  Module tickets
   --  ------------------------------
   type Ticket_Module is new AWA.Modules.Module with private;
   type Ticket_Module_Access is access all Ticket_Module'Class;

   --  Initialize the tickets module.
   overriding
   procedure Initialize (Plugin : in out Ticket_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the tickets module.
   function Get_Ticket_Module return Ticket_Module_Access;

   --  Load the ticket.
   procedure Load_Ticket (Model    : in Ticket_Module;
                          Ticket   : in out Jason.Tickets.Models.Ticket_Ref'Class;
                          Project  : in out Jason.Projects.Models.Project_Ref'Class;
                          Tags     : in out AWA.Tags.Beans.Tag_List_Bean;
                          Id       : in ADO.Identifier);

   --  Create
   procedure Create (Model      : in Ticket_Module;
                     Entity     : in out Jason.Tickets.Models.Ticket_Ref'Class;
                     Project_Id : in ADO.Identifier);

   --  Save
   procedure Save (Model   : in Ticket_Module;
                   Entity  : in out Jason.Tickets.Models.Ticket_Ref'Class;
                   Comment : in String);
private

   type Ticket_Module is new AWA.Modules.Module with null record;

end Jason.Tickets.Modules;
