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

with ASF.Events.Faces.Actions;
package body Jason.Tickets.Beans is

   --  ------------------------------
   --  Example of action method.
   --  ------------------------------
   procedure Action (Bean    : in out Ticket_Bean;
                     Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is
   begin
      null;
   end Action;

   package Action_Binding is
     new ASF.Events.Faces.Actions.Action_Method.Bind (Bean   => Ticket_Bean,
                                                      Method => Action,
                                                      Name   => "action");

   Ticket_Bean_Binding : aliased constant Util.Beans.Methods.Method_Binding_Array
     := (Action_Binding.Proxy'Access, null);

   --  ------------------------------
   --  Get the value identified by the name.
   --  ------------------------------
   overriding
   function Get_Value (From : in Ticket_Bean;
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
   procedure Set_Value (From  : in out Ticket_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object) is
   begin
      if Name = "count" then
         From.Count := Util.Beans.Objects.To_Integer (Value);
      end if;
   end Set_Value;

   --  ------------------------------
   --  This bean provides some methods that can be used in a Method_Expression
   --  ------------------------------
   overriding
   function Get_Method_Bindings (From : in Ticket_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access is
      pragma Unreferenced (From);
   begin
      return Ticket_Bean_Binding'Access;
   end Get_Method_Bindings;

   --  ------------------------------
   --  Create the Ticket_Bean bean instance.
   --  ------------------------------
   function Create_Ticket_Bean (Module : in Jason.Tickets.Modules.Ticket_Module_Access)
      return Util.Beans.Basic.Readonly_Bean_Access is
      Object : constant Ticket_Bean_Access := new Ticket_Bean;
   begin
      Object.Module := Module;
      return Object.all'Access;
   end Create_Ticket_Bean;

end Jason.Tickets.Beans;
