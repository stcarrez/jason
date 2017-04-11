-----------------------------------------------------------------------
--  jason-projects-beans -- Beans for module projects
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
with AWA.Services.Contexts;
with AWA.Tags.Modules;
with AWA.Wikis.Modules;
with ADO.Sessions;
with ADO.Sessions.Entities;
with ADO.Queries;
with ADO.Utils;
with ADO.Datasets;
with ADO.Parameters;
package body Jason.Projects.Beans is

   --  ------------------------------
   --  Create project action.
   --  ------------------------------
   overriding
   procedure Create (Bean    : in out Project_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Create (Bean);
      Bean.Tags.Update_Tags (Bean.Get_Id);
   end Create;

   --  ------------------------------
   --  Save project action.
   --  ------------------------------
   overriding
   procedure Save (Bean    : in out Project_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Save (Bean);
      Bean.Tags.Update_Tags (Bean.Get_Id);
   end Save;

   --  ------------------------------
   --  Load project information.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Project_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is

      use type ADO.Identifier;

   begin
      if Bean.Id = ADO.NO_IDENTIFIER then
         Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("failed");
         return;
      end if;
      Bean.Module.Load_Project (Bean, Bean.Wiki_Space, Bean.Tags, Bean.Id, ADO.NO_IDENTIFIER);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
   end Load;

   --  ------------------------------
   --  Create the wiki space.
   --  ------------------------------
   overriding
   procedure Create_Wiki (Bean    : in out Project_Bean;
                          Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      Bean.Module.Create_Wiki (Bean, Bean.Wiki_Space);
   end Create_Wiki;

   --  Load the project if it is associated with the current wiki space.
   overriding
   procedure Load_Wiki (Bean    : in out Project_Bean;
                        Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type ADO.Identifier;
      Page : AWA.Wikis.Beans.Wiki_View_Bean_Access
        := AWA.Wikis.Beans.Get_Wiki_View_Bean ("wikiView");
   begin
      --  Page.Load (Outcome);
      if not Page.Wiki_Space.Is_Null then
         Bean.Module.Load_Project (Bean, Bean.Wiki_Space, Bean.Tags, ADO.NO_IDENTIFIER,
                                   Page.Wiki_Space.Get_Id);
      end if;
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
   end Load_Wiki;

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Project_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
   begin
      if Name = "count" then
         return Util.Beans.Objects.To_Object (From.Count);
      elsif Name = "tags" then
         return Util.Beans.Objects.To_Object (From.Tags_Bean, Util.Beans.Objects.STATIC);
      elsif NAme ="has_wiki" then
         return Util.Beans.Objects.To_Object (not From.Wiki_Space.Is_Null);
      elsif Name = "wiki" then
         return From.Wiki_Space.Get_Value ("name");
      elsif NAme ="wiki_id" then
         return From.Wiki_Space.Get_Value ("id");
      else
         return Jason.Projects.Models.Project_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  ------------------------------
   --  Set the value identified by the name.
   --  ------------------------------
   overriding
   procedure Set_Value (From  : in out Project_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "id" and not Util.Beans.Objects.Is_Empty (Value) then
         From.Id := ADO.Utils.To_Identifier (Value);
         From.Module.Load_Project (From, From.Wiki_Space, From.Tags, From.Id, ADO.NO_IDENTIFIER);
      elsif Name = "wiki" then
         From.Wiki_Space.Set_Value ("name", Value);
      else
         Jason.Projects.Models.Project_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  Create the Project_Bean bean instance.
   --  ------------------------------
   function Create_Project_Bean (Module : in Jason.Projects.Modules.Project_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Project_Bean_Access := new Project_Bean;
   begin
      Object.Module := Module;
      Object.Tags_Bean := Object.Tags'Access;
      Object.Tags.Set_Entity_Type (Jason.Projects.Models.PROJECT_TABLE);
      Object.Tags.Set_Permission ("project-update");
      Object.Wiki_Space.Module := AWA.Wikis.Modules.Get_Wiki_Module;
      return Object.all'Access;
   end Create_Project_Bean;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Project_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object is
      Pos : Natural;
   begin
      if Name = "tags" then
         Pos := From.Projects.Get_Row_Index;
         if Pos = 0 then
            return Util.Beans.Objects.Null_Object;
         end if;
         declare
            Item : constant Models.List_Info := From.Projects.List.Element (Pos - 1);
         begin
            return From.Tags.Get_Tags (Item.Id);
         end;
      elsif Name = "page_count" then
         return Util.Beans.Objects.To_Object ((From.Count + From.Page_Size - 1) / From.Page_Size);
      elsif Name = "projects" then
         return Util.Beans.Objects.To_Object (Value   => From.Projects_Bean,
                                              Storage => Util.Beans.Objects.STATIC);
      else
         return Jason.Projects.Models.Project_List_Bean (From).Get_Value (Name);
      end if;
   end Get_Value;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Project_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if not Util.Beans.Objects.Is_Empty (Value) or else Name = "tags" then
         Jason.Projects.Models.Project_List_Bean (From).Set_Value (Name, Value);
      end if;
   end Set_Value;

   --  Load list of projects.
   overriding
   procedure Load (Bean    : in out Project_List_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
      use type ADO.Identifier;
      use Ada.Strings.Unbounded;
      use Jason.Projects.Models;

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
         Query.Set_Query (Jason.Projects.Models.Query_List_Tag_Filter);
         Query.Bind_Param (Name => "tag", Value => Tag_Id);
         Count_Query.Set_Count_Query (Jason.Projects.Models.Query_List_Tag_Filter);
         Count_Query.Bind_Param (Name => "tag", Value => Tag_Id);
      else
         Query.Set_Query (Jason.Projects.Models.Query_List);
         Count_Query.Set_Count_Query (Jason.Projects.Models.Query_List);
      end if;
      Query.Bind_Param (Name => "first", Value => First);
      Query.Bind_Param (Name => "count", Value => Bean.Page_Size);
      Query.Bind_Param (Name => "user_id", Value => User);
      Count_Query.Bind_Param (Name => "user_id", Value => User);
      ADO.Sessions.Entities.Bind_Param (Params  => Query,
                                        Name    => "project_table",
                                        Table   => Jason.Projects.Models.PROJECT_TABLE,
                                        Session => Session);
      ADO.Sessions.Entities.Bind_Param (Params  => Count_Query,
                                        Name    => "project_table",
                                        Table   => Jason.Projects.Models.PROJECT_TABLE,
                                        Session => Session);
      Jason.Projects.Models.List (Bean.Projects, Session, Query);
      Bean.Count := ADO.Datasets.Get_Count (Session, Count_Query);
      declare
         List : ADO.Utils.Identifier_Vector;
         Iter : Models.List_Info_Vectors.Cursor := Bean.Projects.List.First;
      begin
         while Models.List_Info_Vectors.Has_Element (Iter) loop
            List.Append (Models.List_Info_Vectors.Element (Iter).Id);
            Models.List_Info_Vectors.Next (Iter);
         end loop;
         Bean.Tags.Load_Tags (Session, Jason.Projects.Models.PROJECT_TABLE.Table.all,
                              List);
      end;
   end Load;

   --  Create the Project_List_Bean bean instance.
   function Create_Project_List_Bean (Module : in Jason.Projects.Modules.Project_Module_Access)
                                     return Util.Beans.Basic.Readonly_Bean_Access is
      Object  : constant Project_List_Bean_Access := new Project_List_Bean;
   begin
      Object.Module     := Module;
      Object.Page_Size  := 20;
      Object.Count      := 0;
      Object.Page       := 1;
      Object.Projects_Bean := Object.Projects'Access;
      return Object.all'Access;
   end Create_Project_List_Bean;

end Jason.Projects.Beans;
