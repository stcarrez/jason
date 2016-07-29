-----------------------------------------------------------------------
--  jason-projects-tests -- Tests for projects
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
with Util.Tests;
with Util.Test_Caller;
with AWA.Tests.Helpers.Users;
with Security.Contexts;
with AWA.Services.Contexts;
with Jason.Projects.Beans;
with Jason.Projects.Modules;
with Jason.Projects.Models;
package body Jason.Projects.Tests is

   package Caller is new Util.Test_Caller (Test, "projects");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test jason.projects.Foo",
                       Test_Create'Access);
   end Add_Tests;

   --  Test project creation.
   procedure Test_Create (T : in out Test) is
      Sec_Ctx   : Security.Contexts.Security_Context;
      Context   : AWA.Services.Contexts.Service_Context;
      P         : Jason.Projects.Beans.Project_Bean;
      Outcome   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      AWA.Tests.Helpers.Users.Login (Context, Sec_Ctx, "test-project@test.com");
      P.Module := Jason.Projects.Modules.Get_Project_Module;
      P.Set_Name ("test-project");
      P.Set_Status (Jason.Projects.Models.OPEN);
      P.Create (Outcome);
      T.Assert (True, "Foo is ok");
   end Test_Create;

end Jason.Projects.Tests;
