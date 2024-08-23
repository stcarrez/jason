-----------------------------------------------------------------------
--  jason-tickets-tests -- Tests for tickets
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

with Util.Test_Caller;
package body Jason.Tickets.Tests is

   package Caller is new Util.Test_Caller (Test, "tickets");

   procedure Add_Tests (Suite : in Util.Tests.Access_Test_Suite) is
   begin
      Caller.Add_Test (Suite, "Test jason.tickets.Foo",
                       Test_Create'Access);
   end Add_Tests;

   procedure Test_Create (T : in out Test) is
   begin
      T.Assert (True, "Foo is ok");
   end Test_Create;

end Jason.Tickets.Tests;
