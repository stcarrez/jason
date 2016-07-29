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

package body Jason.Projects.Beans is

   --  ------------------------------
   --  Create project action.
   --  ------------------------------
   overriding
   procedure Create (Bean    : in out Project_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Create;

   --  ------------------------------
   --  Load project information.
   --  ------------------------------
   overriding
   procedure Load (Bean    : in out Project_Bean;
                   Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
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
      else
         return Util.Beans.Objects.Null_Object;
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
      if Name = "count" then
         From.Count := Util.Beans.Objects.To_Integer (Value);
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
      return Object.all'Access;
   end Create_Project_Bean;

end Jason.Projects.Beans;
