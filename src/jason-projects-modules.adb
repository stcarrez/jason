-----------------------------------------------------------------------
--  jason-projects-modules -- Module projects
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
with Jason.Projects.Beans;
package body Jason.Projects.Modules is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Jason.Projects.Module");

   package Register is new AWA.Modules.Beans (Module => Project_Module,
                                              Module_Access => Project_Module_Access);

   --  ------------------------------
   --  Initialize the projects module.
   --  ------------------------------
   overriding
   procedure Initialize (Plugin : in out Project_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config) is
   begin
      Log.Info ("Initializing the projects module");

      --  Register here any bean class, servlet, filter.
      Register.Register (Plugin => Plugin,
                         Name   => "Jason.Projects.Beans.Projects_Bean",
                         Handler => Jason.Projects.Beans.Create_Project_Bean'Access);

      AWA.Modules.Module (Plugin).Initialize (App, Props);

      --  Add here the creation of manager instances.
   end Initialize;

   --  ------------------------------
   --  Get the projects module.
   --  ------------------------------
   function Get_Project_Module return Project_Module_Access is
      function Get is new AWA.Modules.Get (Project_Module, Project_Module_Access, NAME);
   begin
      return Get;
   end Get_Project_Module;

end Jason.Projects.Modules;
