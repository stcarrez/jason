-----------------------------------------------------------------------
--  jason-testsuite -- Testsuite for jason
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

with AWA.Tests;
with ASF.Server.Tests;
with Jason.Applications;
with Jason.Projects.Tests;
package body Jason.Testsuite is

   Tests : aliased Util.Tests.Test_Suite;

   function Suite return Util.Tests.Access_Test_Suite is
   begin
      Jason.Projects.Tests.Add_Tests (Tests'Access);
      return Tests'Access;
   end Suite;

   procedure Initialize (Props : in Util.Properties.Manager) is
      App  : constant Jason.Applications.Application_Access := Jason.Applications.Create;
   begin
--      Jason.Applications.Initialize (App);
--        ASF.Server.Tests.Set_Context (App.all'Access);
      AWA.Tests.Initialize (App.all'Access, Props, True);
   end Initialize;

end Jason.Testsuite;
