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

with ADO.Utils;
with ADO.Queries;
with ADO.Sessions;
with ADO.Datasets;
with ADO.Parameters;
with ADO.Sessions.Entities;
with AWA.Tags.Modules;
with AWA.Services.Contexts;
with AWA.Helpers.Selectors;
with Jason.Projects.Models;
package body Jason.Tickets.Beans is

   function Create_From_Status is
     new AWA.Helpers.Selectors.Create_From_Enum (Jason.Tickets.Models.Status_Type,
                                                 "ticket_status");

   function Create_From_Ticket_Type is
     new AWA.Helpers.Selectors.Create_From_Enum (Jason.Tickets.Models.Ticket_Type,
                                                 "ticket_type");

   --  ------------------------------
   --  Get a select item list which contains a list of ticket status.
   --  ------------------------------
   function Create_Status_List (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);
      use AWA.Helpers;
   begin
      return Selectors.Create_Selector_Bean (Bundle  => "tickets",
                                             Context => null,
                                             Create  => Create_From_Status'Access).all'Access;
   end Create_Status_List;

   --  ------------------------------
   --  Get a select item list which contains a list of ticket types.
   --  ------------------------------
   function Create_Type_List (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
                                return Util.Beans.Basic.Readonly_Bean_Access is
      pragma Unreferenced (Module);
      use AWA.Helpers;
   begin
      return Selectors.Create_Selector_Bean (Bundle  => "tickets",
                                             Context => null,
                                             Create  => Create_From_Ticket_Type'Access).all'Access;
   end Create_Type_List;

   --  ------------------------------
   --  Create ticket action.
   --  ------------------------------
   overriding
   procedure Create (Bean    : in out Ticket_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Create (Bean, Bean.Project_Id);
      Bean.Tags.Update_Tags (Bean.Get_Id);
   end Create;

   --  Save ticket action.
   overriding
   procedure Save (Bean    : in out Ticket_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Save (Bean);
      Bean.Tags.Update_Tags (Bean.Get_Id);
   end Save;

   --  Load ticket action.
   overriding
   procedure Load (Bean    : in out Ticket_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Load_Ticket (Bean, Bean.Project, Bean.Tags, Bean.Ticket_Id);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
   end Load;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Ticket_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "project_id" then
         if From.Project.Is_Inserted then
            return ADO.Utils.To_Object (From.Project.Get_Id);
         else
            return ADO.Utils.To_Object (From.Project_Id);
         end if;
      elsif Name = "ticket_id" or Name = "id" then
         if From.Is_Inserted or From.Is_Loaded then
            return ADO.Utils.To_Object (From.Get_Id);
         else
            return ADO.Utils.To_Object (From.Ticket_Id);
         end if;
      elsif Name = "tags" then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
      else
         return Jason.Tickets.Models.Ticket_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Ticket_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "project_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Project_Id := ADO.Utils.To_Identifier (Value);
      elsif Name = "ticket_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Ticket_Id := ADO.Utils.To_Identifier (Value);
      elsif Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Ticket_Id := ADO.Utils.To_Identifier (Value);
         From.Module.Load_Ticket (From, From.Project, From.Tags, From.Ticket_Id);
      else
         Jason.Tickets.Models.Ticket_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create the Ticket_Bean bean instance.
   --  ------------------------------
   function Create_Ticket_Bean (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Ticket_Bean_Access := new Ticket_Bean;
   begin
      Object.Module := Module;
      Object.Tags_Bean := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (Jason.Tickets.Models.TICKET_TABLE);
      Object.Tags.Set_Permission ("ticket-update");
      return Object.all'Access;
   end Create_Ticket_Bean;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Ticket_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      Pos : Natural;
   begin
      if Name = "tags" then
         Pos := From.Tickets.Get_Row_Index;
         if Pos = 0 then
            return Util.Beans.Objects.Null_Object;
         end if;
         declare
            Item : constant Models.List_Info := From.Tickets.List.Element (Pos - 1);
         begin
            return From.Tags.Get_Tags (Item.Id);
         end;
      elsif Name = "page_count" then
         return Util.Beans.Objects.To_Object ((From.Count + From.Page_Size - 1) / From.Page_Size);
      elsif Name = "tickets" then
         return Util.Beans.Objects.To_Object (Value   => From.Tickets_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      else
         return Jason.Tickets.Models.Ticket_List_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Ticket_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if not Util.Beans.Objects.Is_Empty (Value) or else Name = "tags" then
         Jason.Tickets.Models.Ticket_List_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  Load list of tickets.
   overriding
   procedure Load (Bean    : in out Ticket_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type ADO.Identifier;
      use Ada.Strings.Unbounded;
      use Jason.Tickets.Models;

      Ctx         : constant AWA.Services.Contexts.Service_Context_Access := AWA.Services.Contexts.Current;
      User        : constant ADO.Identifier := Ctx.Get_User_Identifier;
      Session     : ADO.Sessions.Session := Bean.Module.Get_Session;
      Query       : ADO.Queries.Context;
      Count_Query : ADO.Queries.Context;
      Tag_Id      : ADO.Identifier;
      First       : constant Natural  := (Bean.Page - 1) * Bean.Page_Size;
      Filter      : Ada.Strings.Unbounded.Unbounded_String;
   begin
      AWA.Tags.Modules.Find_Tag_Id (Session, Ada.Strings.Unbounded.To_String (Bean.Tag), Tag_Id);
      if Tag_Id /= ADO.NO_IDENTIFIER then
         Query.Set_Query (Jason.Tickets.Models.Query_List_Tag_Filter);
         Query.Bind_Param (Name => "tag", Value => Tag_Id);
         Count_Query.Set_Count_Query (Jason.Tickets.Models.Query_List_Tag_Filter);
         Count_Query.Bind_Param (Name => "tag", Value => Tag_Id);
      else
         Query.Set_Query (Jason.Tickets.Models.Query_List);
         Count_Query.Set_Count_Query (Jason.Tickets.Models.Query_List);
      end if;
      if Bean.Sort = "newest" then
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("ticket.create_date DESC"));
      elsif Bean.Sort = "oldest" then
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("ticket.create_date ASC"));
      elsif Bean.Sort = "recent" then
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("ticket.update_date DESC"));
      elsif Bean.Sort = "not-recent" then
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("ticket.update_date ASC"));
      else
         Query.Bind_Param (Name  => "order1",
                           Value => ADO.Parameters.Token '("ticket.ident DESC"));
      end if;
      if Bean.Priority > 0 then
         Ada.Strings.Unbounded.Append (Filter, "ticket.priority = ");
         Ada.Strings.Unbounded.Append (Filter, Natural'Image (Bean.Priority));
         Ada.Strings.Unbounded.Append (Filter, " AND ");
      end if;
      if Bean.Status /= Jason.Tickets.Models.OPEN then
         Ada.Strings.Unbounded.Append (Filter, "ticket.status = ");
         Ada.Strings.Unbounded.Append (Filter, Natural'Image (Jason.Tickets.Models.Status_Type'Pos (Bean.Status)));
      else
         Ada.Strings.Unbounded.Append (Filter, "ticket.status >= 0");
      end if;
      Query.Bind_Param (Name => "ticket_filter",
                        Value => ADO.Parameters.Token (To_String (Filter)));
      Count_Query.Bind_Param (Name => "ticket_filter",
                              Value => ADO.Parameters.Token (To_String (Filter)));
      Query.Bind_Param (Name => "project_id", Value => Bean.Project_Id);
      Query.Bind_Param (Name => "first", Value => First);
      Query.Bind_Param (Name => "count", Value => Bean.Page_Size);
      Query.Bind_Param (Name => "user_id", Value => User);
      Count_Query.Bind_Param (Name => "project_id", Value => Bean.Project_Id);
      Count_Query.Bind_Param (Name => "user_id", Value => User);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "ticket_table",
                                        Table   => Jason.Tickets.Models.TICKET_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "project_table",
                                        Table   => Jason.Projects.Models.PROJECT_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Count_Query,
                                        Name    => "ticket_table",
                                        Table   => Jason.Tickets.Models.TICKET_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Count_Query,
                                        Name    => "project_table",
                                        Table   => Jason.Projects.Models.PROJECT_TABLE,
                                        Session => Session);
      Jason.Tickets.Models.List (Bean.Tickets, Session, Query);
      Bean.Count := ADO.Datasets.Get_Count (Session, Count_Query);
      declare
         List : ADO.Utils.Identifier_Vector;
         Iter : Models.List_Info_Vectors.Cursor := Bean.Tickets.List.First;
      begin
         while Models.List_Info_Vectors.Has_Element (Iter) loop
            List.Append (Models.List_Info_Vectors.Element (Iter).Id);
            Models.List_Info_Vectors.Next (Iter);
         end loop;
         Bean.Tags.Load_Tags (Session, Jason.Tickets.Models.TICKET_TABLE.Table.all,
                              List);
      end;
   end Load;

   --  Create the Tickets_List_Bean bean instance.
   function Create_Ticket_List_Bean (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Ticket_List_Bean_Access := new Ticket_List_Bean;
   begin
      Object.Module     := Module;
      Object.Page_Size  := 20;
      Object.Count      := 0;
      Object.Page       := 1;
      Object.Priority   := -1;
      Object.Project_Id := ADO.NO_IDENTIFIER;
      Object.Status     := Jason.Tickets.Models.OPEN;
      Object.Tickets_Bean := Object.Tickets'Access;
      return Object.all'Access;
   end Create_Ticket_List_Bean;

end Jason.Tickets.Beans;
