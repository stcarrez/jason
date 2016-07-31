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
with ADO.Sessions;
with ADO.Queries;
with ADO.Utils;
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
      Bean.Module.Load_Project (Bean, Bean.Tags, Bean.Id);
      Outcome := Ada.Strings.Unbounded.To_Unbounded_String ("loaded");
   end Load;

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
         From.Module.Load_Project (From, From.Tags, From.Id);
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
      return Object.all'Access;
   end Create_Project_Bean;

end Jason.Projects.Beans;
