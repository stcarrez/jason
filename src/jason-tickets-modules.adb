-----------------------------------------------------------------------
--  jason-tickets-modules -- Module tickets
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

with AWA.Modules.Beans;
with AWA.Modules.Get;
with Util.Log.Loggers;
with Jason.Tickets.Beans;
package body Jason.Tickets.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Jason.Tickets.Module");

   package Register is new AWA.Modules.Beans (Module => Ticket_Module,
                                              Module_Access => Ticket_Module_Access);

   --  ------------------------------
   --  Initialize the tickets module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Ticket_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the tickets module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Tickets.Beans.Tickets_Bean",
                         Handler => Jason.Tickets.Beans.Create_Ticket_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the tickets module.
   --  ------------------------------
   function Get_Ticket_Module return Ticket_Module_Access is
      function Get is new AWA.Modules.Get (Ticket_Module, Ticket_Module_Access, NAME);
   begin
      return Get;
   end Get_Ticket_Module;

end Jason.Tickets.Modules;
