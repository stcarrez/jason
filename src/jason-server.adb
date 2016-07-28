-----------------------------------------------------------------------
--  Jason-server -- Application server
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
with Ada.Exceptions;

with Util.Log.loggers;

with ASF.Server.Web;

with Jason.Applications;
procedure Jason.Server is

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Jason.Server");
   App     : constant Jason.Applications.Application_Access := new Jason.Applications.Application;
   WS      : ASF.Server.Web.AWS_Container;
begin
   Jason.Applications.Initialize (App);
   WS.Register_Application (Jason.Applications.CONTEXT_PATH, App.all'Access);
   Log.Info ("Connect you browser to: http://localhost:8080/jason/index.html");
   WS.Start;
   delay 365.0 * 24.0 * 3600.0;
   App.Close;
exception
   when E: others =>
      Log.Error ("Exception in server: " &
                 Ada.Exceptions.Exception_Name (E) & ": " &
                 Ada.Exceptions.Exception_Message (E));
end Jason.Server;
