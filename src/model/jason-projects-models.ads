-----------------------------------------------------------------------
--  Jason.Projects.Models -- Jason.Projects.Models
-----------------------------------------------------------------------
--  File generated by Dynamo DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://github.com/stcarrez/dynamo Version 1.4.0
-----------------------------------------------------------------------
--  Copyright (C) 2023 Stephane.Carrez
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
pragma Warnings (Off);
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with ADO.Queries;
with ADO.Queries.Loaders;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Objects.Enums;
with Util.Beans.Basic.Lists;
with AWA.Users.Models;
with AWA.Wikis.Models;
with Util.Beans.Methods;
pragma Warnings (On);
package Jason.Projects.Models is

   pragma Style_Checks ("-mrIu");

   type Status_Type is (OPEN, CLOSED, ON_HOLD);
   for Status_Type use (OPEN => 0, CLOSED => 1, ON_HOLD => 2);
   package Status_Type_Objects is
      new Util.Beans.Objects.Enums (Status_Type);

   type Nullable_Status_Type is record
      Is_Null : Boolean := True;
      Value   : Status_Type;
   end record;

   type Project_Ref is new ADO.Objects.Object_Ref with null record;

   type Attribute_Definition_Ref is new ADO.Objects.Object_Ref with null record;

   --  --------------------
   --  The project describes the base information for the project management.
   --  --------------------
   --  Create an object key for Project.
   function Project_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Project from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Project_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Project : constant Project_Ref;
   function "=" (Left, Right : Project_Ref'Class) return Boolean;

   --  Set the project identifier
   procedure Set_Id (Object : in out Project_Ref;
                     Value  : in ADO.Identifier);

   --  Get the project identifier
   function Get_Id (Object : in Project_Ref)
                 return ADO.Identifier;
   --  Get the optimistic lock version
   function Get_Version (Object : in Project_Ref)
                 return Integer;

   --  Set the project name
   procedure Set_Name (Object : in out Project_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Project_Ref;
                       Value : in String);

   --  Get the project name
   function Get_Name (Object : in Project_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Project_Ref)
                 return String;

   --  Set the project creation date.
   procedure Set_Create_Date (Object : in out Project_Ref;
                              Value  : in Ada.Calendar.Time);

   --  Get the project creation date.
   function Get_Create_Date (Object : in Project_Ref)
                 return Ada.Calendar.Time;

   --  Set the project status.
   procedure Set_Status (Object : in out Project_Ref;
                         Value  : in Status_Type);

   --  Get the project status.
   function Get_Status (Object : in Project_Ref)
                 return Status_Type;

   --  Set the last ticket number that was allocated.
   procedure Set_Last_Ticket (Object : in out Project_Ref;
                              Value  : in Integer);

   --  Get the last ticket number that was allocated.
   function Get_Last_Ticket (Object : in Project_Ref)
                 return Integer;

   --
   procedure Set_Update_Date (Object : in out Project_Ref;
                              Value  : in Ada.Calendar.Time);

   --
   function Get_Update_Date (Object : in Project_Ref)
                 return Ada.Calendar.Time;

   --  Set the project description.
   procedure Set_Description (Object : in out Project_Ref;
                              Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Description (Object : in out Project_Ref;
                              Value : in String);

   --  Get the project description.
   function Get_Description (Object : in Project_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Description (Object : in Project_Ref)
                 return String;

   --
   procedure Set_Wiki (Object : in out Project_Ref;
                       Value  : in AWA.Wikis.Models.Wiki_Space_Ref'Class);

   --
   function Get_Wiki (Object : in Project_Ref)
                 return AWA.Wikis.Models.Wiki_Space_Ref'Class;

   --  Set the project owner.
   procedure Set_Owner (Object : in out Project_Ref;
                        Value  : in AWA.Users.Models.User_Ref'Class);

   --  Get the project owner.
   function Get_Owner (Object : in Project_Ref)
                 return AWA.Users.Models.User_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Project_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Project_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Reload from the database the same object if it was modified.
   --  Returns True in `Updated` if the object was reloaded.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Reload (Object  : in out Project_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Project_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Project_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Project_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Project_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   PROJECT_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Project_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Project_Ref;
                   Into   : in out Project_Ref);

   package Project_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Project_Ref,
                                  "="          => "=");
   subtype Project_Vector is Project_Vectors.Vector;

   procedure List (Object  : in out Project_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class);
   --  Create an object key for Attribute_Definition.
   function Attribute_Definition_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Attribute_Definition from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Attribute_Definition_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Attribute_Definition : constant Attribute_Definition_Ref;
   function "=" (Left, Right : Attribute_Definition_Ref'Class) return Boolean;

   --  Set the attribute identifier.
   procedure Set_Id (Object : in out Attribute_Definition_Ref;
                     Value  : in ADO.Identifier);

   --  Get the attribute identifier.
   function Get_Id (Object : in Attribute_Definition_Ref)
                 return ADO.Identifier;
   --  Get the optimistic lock version.
   function Get_Version (Object : in Attribute_Definition_Ref)
                 return Integer;

   --  Set the attribute name or label.
   procedure Set_Name (Object : in out Attribute_Definition_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Attribute_Definition_Ref;
                       Value : in String);

   --  Get the attribute name or label.
   function Get_Name (Object : in Attribute_Definition_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Attribute_Definition_Ref)
                 return String;

   --  Set the default value.
   procedure Set_Default_Value (Object : in out Attribute_Definition_Ref;
                                Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Default_Value (Object : in out Attribute_Definition_Ref;
                                Value : in String);

   --  Get the default value.
   function Get_Default_Value (Object : in Attribute_Definition_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Default_Value (Object : in Attribute_Definition_Ref)
                 return String;

   --
   procedure Set_Project (Object : in out Attribute_Definition_Ref;
                          Value  : in Project_Ref'Class);

   --
   function Get_Project (Object : in Attribute_Definition_Ref)
                 return Project_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Attribute_Definition_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Attribute_Definition_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Reload from the database the same object if it was modified.
   --  Returns True in `Updated` if the object was reloaded.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Reload (Object  : in out Attribute_Definition_Ref;
                     Session : in out ADO.Sessions.Session'Class;
                     Updated : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Attribute_Definition_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Attribute_Definition_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Attribute_Definition_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Attribute_Definition_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   ATTRIBUTE_DEFINITION_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Attribute_Definition_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Attribute_Definition_Ref;
                   Into   : in out Attribute_Definition_Ref);

   package Attribute_Definition_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Attribute_Definition_Ref,
                                  "="          => "=");
   subtype Attribute_Definition_Vector is Attribute_Definition_Vectors.Vector;

   procedure List (Object  : in out Attribute_Definition_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class);

   --  --------------------
   --    The list of projects.
   --  --------------------
   type List_Info is
     new Util.Beans.Basic.Bean with  record

      --  the project identifier.
      Id : ADO.Identifier;

      --  the project title.
      Title : Ada.Strings.Unbounded.Unbounded_String;

      --  the project status.
      Status : Status_Type;

      --  the project creation date.
      Create_Date : Ada.Calendar.Time;

      --  the total duration for tickets.
      Total_Duration : Natural;

      --  the total progress time.
      Total_Done : Float;

      --  the number of tickets closed.
      Close_Count : Natural;

      --  the number of tickets opened.
      Open_Count : Natural;
   end record;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in List_Info;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out List_Info;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);


   package List_Info_Beans is
      new Util.Beans.Basic.Lists (Element_Type => List_Info);
   package List_Info_Vectors renames List_Info_Beans.Vectors;
   subtype List_Info_List_Bean is List_Info_Beans.List_Bean;

   type List_Info_List_Bean_Access is access all List_Info_List_Bean;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out List_Info_List_Bean'Class;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   subtype List_Info_Vector is List_Info_Vectors.Vector;

   --  Run the query controlled by <b>Context</b> and append the list in <b>Object</b>.
   procedure List (Object  : in out List_Info_Vector;
                   Session : in out ADO.Sessions.Session'Class;
                   Context : in out ADO.Queries.Context'Class);

   Query_List : constant ADO.Queries.Query_Definition_Access;

   Query_List_Tag_Filter : constant ADO.Queries.Query_Definition_Access;


   --  --------------------
   --    create the wiki space associated with the project.
   --  --------------------
   type Project_Bean is abstract new Jason.Projects.Models.Project_Ref
     and Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with null record;


   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Project_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;


   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Project_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Load (Bean : in out Project_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Create (Bean : in out Project_Bean;
                    Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Save (Bean : in out Project_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Create_Wiki (Bean : in out Project_Bean;
                         Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   procedure Load_Wiki (Bean : in out Project_Bean;
                       Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;

   --  --------------------
   --    load the project list.
   --  --------------------
   type Project_List_Bean is abstract limited
     new Util.Beans.Basic.Bean and Util.Beans.Methods.Method_Bean with  record

      --  the current page number for the pagination.
      Page : Integer;

      --  the number of projects in a page.
      Page_Size : Integer;

      --  the number of projects.
      Count : Integer;

      --  the optional tag to filter the list of projects.
      Tag : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   --  This bean provides some methods that can be used in a Method_Expression.
   overriding
   function Get_Method_Bindings (From : in Project_List_Bean)
                                 return Util.Beans.Methods.Method_Binding_Array_Access;

   --  Get the bean attribute identified by the name.
   overriding
   function Get_Value (From : in Project_List_Bean;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Set the bean attribute identified by the name.
   overriding
   procedure Set_Value (Item  : in out Project_List_Bean;
                        Name  : in String;
                        Value : in Util.Beans.Objects.Object);

   procedure Load (Bean : in out Project_List_Bean;
                  Outcome : in out Ada.Strings.Unbounded.Unbounded_String) is abstract;


private
   PROJECT_NAME : aliased constant String := "jason_project";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "version";
   COL_2_1_NAME : aliased constant String := "name";
   COL_3_1_NAME : aliased constant String := "create_date";
   COL_4_1_NAME : aliased constant String := "status";
   COL_5_1_NAME : aliased constant String := "last_ticket";
   COL_6_1_NAME : aliased constant String := "update_date";
   COL_7_1_NAME : aliased constant String := "description";
   COL_8_1_NAME : aliased constant String := "wiki_id";
   COL_9_1_NAME : aliased constant String := "owner_id";

   PROJECT_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 10,
      Table   => PROJECT_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access,
         5 => COL_4_1_NAME'Access,
         6 => COL_5_1_NAME'Access,
         7 => COL_6_1_NAME'Access,
         8 => COL_7_1_NAME'Access,
         9 => COL_8_1_NAME'Access,
         10 => COL_9_1_NAME'Access)
     );
   PROJECT_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := PROJECT_DEF'Access;


   Null_Project : constant Project_Ref
      := Project_Ref'(ADO.Objects.Object_Ref with null record);

   type Project_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => PROJECT_DEF'Access)
   with record
       Version : Integer;
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Create_Date : Ada.Calendar.Time;
       Status : Status_Type;
       Last_Ticket : Integer;
       Update_Date : Ada.Calendar.Time;
       Description : Ada.Strings.Unbounded.Unbounded_String;
       Wiki : AWA.Wikis.Models.Wiki_Space_Ref;
       Owner : AWA.Users.Models.User_Ref;
   end record;

   type Project_Access is access all Project_Impl;

   overriding
   procedure Destroy (Object : access Project_Impl);

   overriding
   procedure Find (Object  : in out Project_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Project_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Project_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Project_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Project_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Project_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Project_Ref'Class;
                        Impl   : out Project_Access);
   ATTRIBUTE_DEFINITION_NAME : aliased constant String := "jason_attribute_definition";
   COL_0_2_NAME : aliased constant String := "id";
   COL_1_2_NAME : aliased constant String := "version";
   COL_2_2_NAME : aliased constant String := "name";
   COL_3_2_NAME : aliased constant String := "default_value";
   COL_4_2_NAME : aliased constant String := "project_id";

   ATTRIBUTE_DEFINITION_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count   => 5,
      Table   => ATTRIBUTE_DEFINITION_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access,
         4 => COL_3_2_NAME'Access,
         5 => COL_4_2_NAME'Access)
     );
   ATTRIBUTE_DEFINITION_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := ATTRIBUTE_DEFINITION_DEF'Access;


   Null_Attribute_Definition : constant Attribute_Definition_Ref
      := Attribute_Definition_Ref'(ADO.Objects.Object_Ref with null record);

   type Attribute_Definition_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => ATTRIBUTE_DEFINITION_DEF'Access)
   with record
       Version : Integer;
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Default_Value : Ada.Strings.Unbounded.Unbounded_String;
       Project : Project_Ref;
   end record;

   type Attribute_Definition_Access is access all Attribute_Definition_Impl;

   overriding
   procedure Destroy (Object : access Attribute_Definition_Impl);

   overriding
   procedure Find (Object  : in out Attribute_Definition_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Attribute_Definition_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Attribute_Definition_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Attribute_Definition_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Create (Object  : in out Attribute_Definition_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Attribute_Definition_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Attribute_Definition_Ref'Class;
                        Impl   : out Attribute_Definition_Access);

   package File_1 is
      new ADO.Queries.Loaders.File (Path => "projects-list.xml",
                                    Sha1 => "E126AC9B625FA8FD3B0E06D629189B96BB880B5C");

   package Def_Listinfo_List is
      new ADO.Queries.Loaders.Query (Name => "list",
                                     File => File_1.File'Access);
   Query_List : constant ADO.Queries.Query_Definition_Access
   := Def_Listinfo_List.Query'Access;

   package Def_Listinfo_List_Tag_Filter is
      new ADO.Queries.Loaders.Query (Name => "list-tag-filter",
                                     File => File_1.File'Access);
   Query_List_Tag_Filter : constant ADO.Queries.Query_Definition_Access
   := Def_Listinfo_List_Tag_Filter.Query'Access;
end Jason.Projects.Models;
