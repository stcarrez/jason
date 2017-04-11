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
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with ADO;
with Util.Beans.Basic;
with Util.Beans.Objects;
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

      --  Whether the ticket type filter is enabled.
      Type_Filter   : Boolean := False;
      Status_Filter : Ada.Strings.Unbounded.Unbounded_String;
      Priority_Filter : Ada.Strings.Unbounded.Unbounded_String;
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

   use type Jason.Tickets.Models.Ticket_Type;

   type Ticket_Stat_Bean is new Models.Stat_Bean with record
      Low    : aliased Models.Stat_Bean;
      High   : aliased Models.Stat_Bean;
      Medium : aliased Models.Stat_Bean;
      Closed : aliased Models.Stat_Bean;
   end record;

   package Ticket_Stat_Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Ticket_Stat_Bean);

   package Ticket_Stat_Map is
     new Ada.Containers.Ordered_Maps (Key_Type     => Models.Ticket_Type,
                                      Element_Type => Ticket_Stat_Bean,
                                      "<"          => Models."<",
                                      "="          => "=");

   type Ticket_Raw_Stat_Bean is new Ticket_Stat_Bean with record
      Low_Bean    : Util.Beans.Objects.Object;
      High_Bean   : Util.Beans.Objects.Object;
      Medium_Bean : Util.Beans.Objects.Object;
      Closed_Bean : Util.Beans.Objects.Object;
   end record;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Ticket_Raw_Stat_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   type Ticket_Report_Bean is new Jason.Tickets.Models.Report_Bean
     and Util.Beans.Basic.List_Bean with record
      Module      : Jason.Tickets.Modules.Ticket_Module_Access := null;
      Project     : Jason.Projects.Beans.Project_Bean_Access;
      Row         : Util.Beans.Objects.Object;
      Current     : Ticket_Stat_Vectors.Cursor;
      Current_Pos : Natural := 0;
      Element     : aliased Ticket_Raw_Stat_Bean;
      List        : Ticket_Stat_Map.Map;
      Report      : Ticket_Stat_Vectors.Vector;
   end record;
   type Ticket_Report_Bean_Access is access all Ticket_Report_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Ticket_Report_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Ticket_Report_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  Get the number of elements in the list.
   function Get_Count (From : Ticket_Report_Bean) return Natural;

   --  Set the current row index.  Valid row indexes start at 1.
   overriding
   procedure Set_Row_Index (From  : in out Ticket_Report_Bean;
                            Index : in Natural);

   --  Get the element at the current row index.
   overriding
   function Get_Row (From  : in Ticket_Report_Bean) return Util.Beans.Objects.Object;

   --  Load the information for the tickets.
   overriding
   procedure Load (Bean    : in out Ticket_Report_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Tickets_Report_Bean bean instance.
   function Create_Ticket_Report_Bean (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access;

end Jason.Tickets.Beans;
