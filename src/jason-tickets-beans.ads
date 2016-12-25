-----------------------------------------------------------------------
--  jason-tickets-beans -- Beans for module tickets
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

with Ada.Strings.Unbounded;
with ADO;
with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;
with AWA.Tags.Beans;
with Jason.Tickets.Modules;
with Jason.Tickets.Models;
with Jason.Projects.Models;
with Jason.Projects.Beans;
package Jason.Tickets.Beans is

   type Ticket_Bean is new Jason.Tickets.Models.Ticket_Bean with record
      Module     : Jason.Tickets.Modules.Ticket_Module_Access := null;
      --  Project_Id : ADO.Identifier := ADO.NO_IDENTIFIER;
      Ticket_Id  : ADO.Identifier := ADO.NO_IDENTIFIER;
      --  Project    : Jason.Projects.Models.Project_Ref;
      Project       : Jason.Projects.Beans.Project_Bean_Access;

      --  List of tags associated with the wiki page.
      Tags          : aliased AWA.Tags.Beans.Tag_List_Bean;
      Tags_Bean     : Util.Beans.Basic.Readonly_Bean_Access;
   end record;
   type Ticket_Bean_Access is access all Ticket_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Ticket_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Ticket_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Create ticket action.
   overriding
   procedure Create (Bean    : in out Ticket_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Save ticket action.
   overriding
   procedure Save (Bean    : in out Ticket_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Save ticket action.
   overriding
   procedure Save_Status (Bean    : in out Ticket_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Load ticket action.
   overriding
   procedure Load (Bean    : in out Ticket_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Tickets_Bean bean instance.
   function Create_Ticket_Bean (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

   --  ------------------------------
   --  Bean that collects the list of tickets filtered by tag, priority and project.
   --  ------------------------------
   type Ticket_List_Bean is new Jason.Tickets.Models.Ticket_List_Bean with record
      Module        : Jason.Tickets.Modules.Ticket_Module_Access := null;
      --  Project       : Jason.Projects.Models.Project_Ref;
      Project       : Jason.Projects.Beans.Project_Bean_Access;

      --  List of tickets.
      Tickets       : aliased Jason.Tickets.Models.List_Info_List_Bean;
      Tickets_Bean  : Jason.Tickets.Models.List_Info_List_Bean_Access;

      --  List of tags associated with the tickets.
      Tags          : AWA.Tags.Beans.Entity_Tag_Map;
   end record;
   type Ticket_List_Bean_Access is access all Ticket_List_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Ticket_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Ticket_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Load list of tickets.
   overriding
   procedure Load (Bean    : in out Ticket_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Tickets_List_Bean bean instance.
   function Create_Ticket_List_Bean (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

   --  Get a select item list which contains a list of ticket status.
   function Create_Status_List (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access;

   --  Get a select item list which contains a list of ticket types.
   function Create_Type_List (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access;

end Jason.Tickets.Beans;
