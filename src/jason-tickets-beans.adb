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
with Jason.Projects.Models;
package body Jason.Tickets.Beans is

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

end Jason.Tickets.Beans;
