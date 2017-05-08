-----------------------------------------------------------------------
--  jason-tickets-beans -- Beans for module tickets
--  Copyright (C) 2016, 2017 Stephane.Carrez
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
with ADO.Statements;
with ADO.Sessions.Entities;
with AWA.Tags.Modules;
with AWA.Users.Models;
with AWA.Services.Contexts;
with AWA.Comments.Beans;
with AWA.Helpers.Selectors;
with Jason.Projects.Models;
package body Jason.Tickets.Beans is

   use type Ada.Strings.Unbounded.Unbounded_String;
   package ASC renames AWA.Services.Contexts;

   function Create_From_Status is
     new AWA.Helpers.Selectors.Create_From_Enum (Jason.Tickets.Models.Status_Type,
                                                 "ticket_status");

   function Create_From_Ticket_Type is
     new AWA.Helpers.Selectors.Create_From_Enum (Jason.Tickets.Models.Ticket_Type,
                                                 "ticket_type");

   procedure Append (Into  : in out Ada.Strings.Unbounded.Unbounded_String;
                     Value : in Jason.Tickets.Models.Status_Type);
   function To_Priority (Value : in Util.Beans.Objects.Object) return Integer;

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
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Create (Bean, Bean.Project.Get_Id);
      Bean.Tags.Update_Tags (Bean.Get_Id);
   end Create;

   --  Save ticket action.
   overriding
   procedure Save (Bean    : in out Ticket_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Save (Bean, Ada.Strings.Unbounded.To_String (Bean.Comment));
      Bean.Tags.Update_Tags (Bean.Get_Id);
   end Save;

   --  Save ticket action.
   overriding
   procedure Save_Status (Bean    : in out Ticket_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
   begin
      Bean.Module.Save (Bean, Ada.Strings.Unbounded.To_String (Bean.Comment));
   end Save_Status;

   --  Load ticket action.
   overriding
   procedure Load (Bean    : in out Ticket_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type AWA.Comments.Beans.Comment_List_Bean_Access;

      Comment_List : AWA.Comments.Beans.Comment_List_Bean_Access;
   begin
      Bean.Module.Load_Ticket (Bean, Bean.Project.all, Bean.Tags, Bean.Ticket_Id);
      Comment_List := AWA.Comments.Beans.Get_Comment_List_Bean ("ticketComments");
      if Comment_List /= null then
         Comment_List.Load_Comments (Bean.Get_Id);
      end if;
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
            return ADO.Utils.To_Object (From.Project.Get_Id);
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
         From.Project.Set_Id (ADO.Utils.To_Identifier (Value));
      elsif Name = "ticket_id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Ticket_Id := ADO.Utils.To_Identifier (Value);
      elsif Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Ticket_Id := ADO.Utils.To_Identifier (Value);
         From.Module.Load_Ticket (From, From.Project.all, From.Tags, From.Ticket_Id);
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
      Object.Project := Jason.Projects.Beans.Get_Project_Bean ("project");
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
      elsif Name = "status" then
         return Util.Beans.Objects.To_Object (From.Status_Filter);
      elsif Name = "priority" then
         if From.Priority_Filter = "" then
            return Util.Beans.Objects.To_Object (String '("all"));
         else
            return Util.Beans.Objects.To_Object (From.Priority_Filter);
         end if;
      else
         return Jason.Tickets.Models.Ticket_List_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   function To_Priority (Value : in Util.Beans.Objects.Object) return Integer is
      S : constant String := Util.Beans.Objects.To_String (Value);
   begin
      if S = "all" or S = "" then
         return 0;
      elsif S = "high" then
         return -3;
      elsif S = "medium" then
         return 4;
      elsif S = "low" then
         return 5;
      else
         return Integer'Value (S);
      end if;

   exception
      when Constraint_Error =>
         return 0;
   end To_Priority;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Ticket_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "priority" then
         From.Priority_Filter := Util.Beans.Objects.To_Unbounded_String (Value);
         From.Priority := To_Priority (Value);
      elsif Name = "ticket_kind" then
         if Util.Beans.Objects.To_String (Value) /= "all"
           and not Util.Beans.Objects.Is_Empty (Value)
         then
            From.Ticket_Kind := Jason.Tickets.Models.Ticket_Type_Objects.To_Value (Value);
            From.Type_Filter := True;
         else
            From.Type_Filter := False;
         end if;
      elsif Name = "status" then
         From.Status_Filter := Util.Beans.Objects.To_Unbounded_String (Value);
         if From.Status_Filter /= "all" and From.Status_Filter /= "pending"
           and From.Status_Filter /= "done" and From.Status_Filter /= ""
         then
            From.Status := Jason.Tickets.Models.Status_Type_Objects.To_Value (Value);
         end if;

      elsif not Util.Beans.Objects.Is_Empty (Value) or else Name = "tags" then
         Jason.Tickets.Models.Ticket_List_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   procedure Append (Into  : in out Ada.Strings.Unbounded.Unbounded_String;
                     Value : in Jason.Tickets.Models.Status_Type) is
   begin
      Ada.Strings.Unbounded.Append (Into, Natural'Image (Models.Status_Type'Pos (Value)));
   end Append;

   --  Load list of tickets.
   overriding
   procedure Load (Bean    : in out Ticket_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type ADO.Identifier;
      use Ada.Strings.Unbounded;
      use Jason.Tickets.Models;

      Ctx         : constant ASC.Service_Context_Access := ASC.Current;
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
      if Bean.Priority /= 0 then
         if Bean.Priority > 0 then
            Ada.Strings.Unbounded.Append (Filter, "ticket.priority = ");
            Ada.Strings.Unbounded.Append (Filter, Natural'Image (Bean.Priority));
         else
            Ada.Strings.Unbounded.Append (Filter, "ticket.priority <= ");
            Ada.Strings.Unbounded.Append (Filter, Natural'Image (-Bean.Priority));
         end if;
         Ada.Strings.Unbounded.Append (Filter, " AND ");
      end if;
      if Bean.Type_Filter then
         Ada.Strings.Unbounded.Append (Filter, "ticket.ticket_type = ");
         Ada.Strings.Unbounded.Append (Filter,
                                       Natural'Image (Models.Ticket_Type'Pos (Bean.Ticket_Kind)));
         Ada.Strings.Unbounded.Append (Filter, " AND ");
      end if;
      if Bean.Status_Filter = "done" then
         Ada.Strings.Unbounded.Append (Filter, "(ticket.status = ");
         Append (Filter, Jason.Tickets.Models.CLOSED);
         Ada.Strings.Unbounded.Append (Filter, " OR ticket.status = ");
         Append (Filter, Jason.Tickets.Models.REJECTED);
         Ada.Strings.Unbounded.Append (Filter, ")");
      elsif Bean.Status_Filter = "all" or Bean.Status_Filter = "" then
         Ada.Strings.Unbounded.Append (Filter, "ticket.status >= 0");
      elsif Bean.Status_Filter = "pending" then
         Ada.Strings.Unbounded.Append (Filter, "ticket.status != ");
         Append (Filter, Jason.Tickets.Models.CLOSED);
         Ada.Strings.Unbounded.Append (Filter, " AND ticket.status != ");
         Append (Filter, Jason.Tickets.Models.REJECTED);
      else
         Ada.Strings.Unbounded.Append (Filter, "ticket.status = ");
         Append (Filter, Bean.Status);
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
      Object.Priority   := 0;
      Object.Project_Id := ADO.NO_IDENTIFIER;
      Object.Status     := Jason.Tickets.Models.OPEN;
      Object.Status_Filter := Ada.Strings.Unbounded.To_Unbounded_String ("pending");
      Object.Tickets_Bean := Object.Tickets'Access;
      Object.Project := Jason.Projects.Beans.Get_Project_Bean ("project");
      return Object.all'Access;
   end Create_Ticket_List_Bean;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Ticket_Raw_Stat_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "low" then
         return From.Low_Bean;
      elsif Name = "medium" then
         return From.Medium_Bean;
      elsif Name = "high" then
         return From.High_Bean;
      elsif Name = "closed" then
         return From.Closed_Bean;
      else
         return Jason.Tickets.Models.Stat_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Ticket_Report_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if name = "count" then
         return Util.Beans.Objects.To_Object (Natural (From.Report.Length));
      end if;
      return Util.Beans.Objects.Null_Object;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Ticket_Report_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      null;
   end Set_Value;

   --  ------------------------------
   --  Get the number of elements in the list.
   --  ------------------------------
   function Get_Count (From : Ticket_Report_Bean) return Natural is
   begin
      return Natural (From.Report.Length);
   end Get_Count;

   --  ------------------------------
   --  Set the current row index.  Valid row indexes start at 1.
   --  ------------------------------
   overriding
   procedure Set_Row_Index (From  : in out Ticket_Report_Bean;
                            Index : in Natural) is
   begin
      if Index = 1 then
         From.Current     := From.Report.First;
         From.Current_Pos := Index;
      elsif Index = Natural (From.Report.Length) then
         From.Current     := From.Report.Last;
         From.Current_Pos := Index;
      else
         while Index > From.Current_Pos and Ticket_Stat_Vectors.Has_Element (From.Current) loop
            Ticket_Stat_Vectors.Next (From.Current);
            From.Current_Pos := From.Current_Pos + 1;
         end loop;
      end if;
      Ticket_Stat_Bean (From.Element) := Ticket_Stat_Vectors.Element (From.Report,
                                                                      From.Current_Pos);
   end Set_Row_Index;

   --  ------------------------------
   --  Get the element at the current row index.
   --  ------------------------------
   overriding
   function Get_Row (From  : in Ticket_Report_Bean) return Util.Beans.Objects.Object is
   begin
      return From.Row;
   end Get_Row;

   --  ------------------------------
   --  Load the information for the tickets.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Ticket_Report_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      pragma Unreferenced (Outcome);
      use AWA.Services;
      use type ADO.Identifier;

      Session    : constant ADO.Sessions.Session := Bean.Module.Get_Session;
      Query      : ADO.Queries.Context;
      Empty      : constant Jason.Tickets.Models.Stat_Bean := (Kind => Models.WORK,
                                                               others => 0);
      Ctx        : constant ASC.Service_Context_Access := ASC.Current;
      User       : constant AWA.Users.Models.User_Ref := Ctx.Get_User;
   begin
      Query.Set_Query (Jason.Tickets.Models.Query_Stats);
      Query.Bind_Param ("project_id", Bean.Project.Get_Id);
      Query.Bind_Param ("user_id", User.Get_Id);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "project_table",
                                        Table   => Jason.Projects.Models.PROJECT_TABLE,
                                        Session => Session);
      declare
         Stmt   : ADO.Statements.Query_Statement := Session.Create_Statement (Query);
      begin
         Stmt.Execute;
         while Stmt.Has_Elements loop
            declare
               use Models;

               Status   : constant Models.Status_Type := Status_Type'Val (Stmt.Get_Integer (0));
               Kind     : constant Models.Ticket_Type := Ticket_Type'Val (Stmt.Get_Integer (1));
               Priority : constant Natural := Stmt.Get_Integer (2);
               Count    : constant Natural := Stmt.Get_Integer (3);
               Time     : constant Natural := Stmt.Get_Integer (4);
               Done     : constant Natural := Stmt.Get_Integer (5);
               Remain   : constant Natural := Stmt.Get_Integer (6);

               procedure Update (Key  : in Models.Ticket_Type;
                                 Item : in out Ticket_Stat_Bean);

               procedure Update (Key  : in Models.Ticket_Type;
                                 Item : in out Ticket_Stat_Bean) is
                  pragma Unreferenced (Key);
               begin
                  if Status = Models.CLOSED or Status = Models.REJECTED then
                     Item.Closed.Count := Item.Closed.Count + Count;
                     Item.Closed.Time  := Item.Closed.Time + Time;
                     Item.Closed.Done  := Item.Closed.Done + Done;
                     Item.Closed.Remain := Item.Closed.Remain + Remain;
                  elsif Priority <= 3 then
                     Item.High.Count := Item.High.Count + Count;
                     Item.High.Time := Item.High.Time + Time;
                     Item.High.Done := Item.High.Done + Done;
                     Item.High.Remain := Item.High.Remain + Remain;
                  elsif Priority > 4 then
                     Item.Low.Count := Item.Low.Count + Count;
                     Item.Low.Time := Item.Low.Time + Time;
                     Item.Low.Done := Item.Low.Done + Done;
                     Item.Low.Remain := Item.Low.Remain + Remain;
                  else
                     Item.Medium.Count := Item.Medium.Count + Count;
                     Item.Medium.Time := Item.Medium.Time + Time;
                     Item.Medium.Done := Item.Medium.Done + Done;
                     Item.Medium.Remain := Item.Medium.Remain + Remain;
                  end if;
               end Update;

               Pos  : constant Ticket_Stat_Map.Cursor := Bean.List.Find (Kind);
            begin
               if Ticket_Stat_Map.Has_Element (Pos) then
                  Bean.List.Update_Element (Pos, Update'Access);
               else
                  declare
                     T : Ticket_Stat_Bean;
                  begin
                     T.Kind     := Kind;
                     T.Count    := 0;
                     T.Time     := 0;
                     T.Remain   := 0;
                     T.High := Empty;
                     T.Low  := Empty;
                     T.Medium := Empty;
                     T.Closed := Empty;
                     Update (Kind, T);
                     Bean.List.Insert (Kind, T);
                  end;
               end if;
            end;
            Stmt.Next;
         end loop;
      end;
      declare
         Iter : Ticket_Stat_Map.Cursor := Bean.List.First;
      begin
         while Ticket_Stat_Map.Has_Element (Iter) loop
            Bean.Report.Append (Ticket_Stat_Map.Element (Iter));
            Ticket_Stat_Map.Next (Iter);
         end loop;
         --  Sort_Tasks.Sort (Into.Tasks);
      end;
   end Load;

   --  ------------------------------
   --  Create the Tickets_Report_Bean bean instance.
   --  ------------------------------
   function Create_Ticket_Report_Bean (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Ticket_Report_Bean_Access := new Ticket_Report_Bean;
   begin
      Object.Module     := Module;
      Object.Project := Jason.Projects.Beans.Get_Project_Bean ("project");
      Object.Row := Util.Beans.Objects.To_Object (Object.Element'Unchecked_Access,
                                                  Util.Beans.Objects.STATIC);
      Object.Element.Low_Bean := Util.Beans.Objects.To_Object (Object.Element.Low'Access,
                                                               Util.Beans.Objects.STATIC);
      Object.Element.High_Bean := Util.Beans.Objects.To_Object (Object.Element.High'Access,
                                                                Util.Beans.Objects.STATIC);
      Object.Element.Medium_Bean := Util.Beans.Objects.To_Object (Object.Element.Medium'Access,
                                                                  Util.Beans.Objects.STATIC);
      Object.Element.Closed_Bean := Util.Beans.Objects.To_Object (Object.Element.Closed'Access,
                                                                  Util.Beans.Objects.STATIC);
      return Object.all'Access;
   end Create_Ticket_Report_Bean;

end Jason.Tickets.Beans;
