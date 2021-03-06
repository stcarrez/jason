-----------------------------------------------------------------------
--  jason-projects-modules -- Module projects
--  Copyright (C) 2016, 2019 Stephane.Carrez
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
with ASF.Applications;
with ADO;
with AWA.Modules;
with AWA.Tags.Beans;
with AWA.Wikis.Models;
with AWA.Wikis.Modules;
with Jason.Projects.Models;
with Security.Permissions;
private with ASF.Converters.Numbers;
package Jason.Projects.Modules is

   --  The name under which the module is registered.
   NAME : constant String := "projects";

   package ACL_View_Projects is new Security.Permissions.Definition ("project-view");
   package ACL_Create_Projects is new Security.Permissions.Definition ("project-create");
   package ACL_Delete_Projects is new Security.Permissions.Definition ("project-delete");
   package ACL_Update_Projects is new Security.Permissions.Definition ("project-update");

   --  ------------------------------
   --  Module projects
   --  ------------------------------
   type Project_Module is new AWA.Modules.Module with private;
   type Project_Module_Access is access all Project_Module'Class;

   --  Initialize the projects module.
   overriding
   procedure Initialize (Plugin : in out Project_Module;
                         App    : in AWA.Modules.Application_Access;
                         Props  : in ASF.Applications.Config);

   --  Get the projects module.
   function Get_Project_Module return Project_Module_Access;


   --  Create
   procedure Create (Model  : in Project_Module;
                     Entity : in out Jason.Projects.Models.Project_Ref'Class);

   --  Save
   procedure Save (Model  : in Project_Module;
                   Entity : in out Jason.Projects.Models.Project_Ref'Class);

   --  Create the project wiki space.
   procedure Create_Wiki (Model  : in Project_Module;
                          Entity : in out Jason.Projects.Models.Project_Ref'Class;
                          Wiki   : in out AWA.Wikis.Models.Wiki_Space_Ref'Class);

   --  Load the project information.
   procedure Load_Project (Model   : in Project_Module;
                           Project : in out Jason.Projects.Models.Project_Ref'Class;
                           Wiki    : in out AWA.Wikis.Models.Wiki_Space_Ref'Class;
                           Tags    : in out AWA.Tags.Beans.Tag_List_Bean;
                           Id      : in ADO.Identifier;
                           Wiki_Id : in ADO.Identifier);

private

   type Project_Module is new AWA.Modules.Module with record
      Wiki              : AWA.Wikis.Modules.Wiki_Module_Access;
      Percent_Converter : aliased ASF.Converters.Numbers.Number_Converter;
      Hour_Converter    : aliased ASF.Converters.Numbers.Number_Converter;
   end record;

end Jason.Projects.Modules;
