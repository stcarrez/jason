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

with Ada.Strings.Unbounded;

with Util.Beans.Basic;
with Util.Beans.Objects;
with Util.Beans.Methods;
with Jason.Projects.Modules;
package Jason.Projects.Beans is

   type Project_Bean is new Util.Beans.Basic.Bean
     and Util.Beans.Methods.Method_Bean with record
      Module : Jason.Projects.Modules.Project_Module_Access := null;
      Count  : Natural := 0;
   end record;
   type Project_Bean_Access is access all Project_Bean'Class;

   --  Get the value identified by the name.
   overriding
   function Get_Value (From : in Project_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the value identified by the name.
   overriding
   procedure Set_Value (From  : in out Project_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   --  This bean provides some methods that can be used in a Method_Expression
   overriding
   function Get_Method_Bindings (From : in Project_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Example of action method.
   procedure Action (Bean    : in out Project_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String);

   --  Create the Projects_Bean bean instance.
   function Create_Project_Bean (Module : in Jason.Projects.Modules.Project_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access;

end Jason.Projects.Beans;
