------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWView is free software; you can redistribute it  and/or modify it under--
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWView is distributed in the hope that it will be useful,but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWView; see file COPYING.  If not, write--
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
------------------------------------------------------------------------------
pragma License (GPL);

------------------------------------------------------------------------------
-- Main package for KOW Text Markup Language processing                     --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_Lib.String_Util;


-------------
-- XML/Ada --
-------------
with DOM.Core;
with DOM.Core.Attrs;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with Input_Sources.File;
with SAX.Readers;
with DOM.Readers;


---------
-- AWS --
---------
with AWS.Utils.Streams;

package body KOW_View.KTML is



	-------------------
	--- Main Methods --
	-------------------


	function Render(
				File_Path	: in String;
				Initial_State	: in KOW_Lib.Json.Object_Type
			) return String is
		-- render the given file into a string

		use Input_Sources.File;
		use SAX.Readers;
		use DOM.Readers;

		Input	: File_Input;
		Reader	: Tree_Reader;
		Doc	: DOM.Core.Document;
		Root	: DOM.Core.Node;

		State	: KOW_Lib.Json.Object_Type := Initial_State;

		Output	: aliased AWS.Utils.Streams.Strings;
	begin

		Set_Public_Id( Input, "Template File" );
		Open( File_Path, Input );

		Set_Feature( Reader, Validation_Feature, False );
		Set_Feature( Reader, Namespace_Feature, False );

		Parse( Reader, Input );
		Close( Input );

		doc := Get_Tree( Reader ); 
		root := DOM.Core.Documents.Get_Element( doc );

		Process_Node( doc, root, state );

		DOM.Core.Nodes.Write(
					Stream		=> Output'Access,
					N		=> doc,
					With_URI	=> False,
					Pretty_Print	=> True
				);
		Free( Reader );

		return AWS.Utils.Streams.Value( Output'Access );
	end Render;

	procedure Process_Node(
			Doc	: in     DOM.Core.Document;
			N	: in out DOM.Core.Node;
			State	: in out KOW_Lib.Json.Object_Type
		) is
		-- query the processor the given tag and process the node
		use DOM.Core;
		Processor : Processors.Processor_Interface'Class := Processors.Get( Elements.Get_Tag_Name( N ) );

	begin
		if Doc.Node_Type = Element_Node then
			Processors.Process_Node(
						Processor	=> Processor,
					       	Doc		=> Doc,
						N		=> N,
    						State		=> State 
					);
		elsif Doc.Node_Type = Text_Node then
			-- TODO :: expand to the attributes (and maybe even tag names)
			Nodes.Set_Node_Value( N, Expand( Nodes.Node_Value( N ), State ) );
		else
			null;
		end if;
	end Process_Node;

	--------------------
	-- Helper Methods --
	--------------------

	function Value_Of(
				Object	: in KOW_Lib.Json.Object_Type;
				Key	: in String
			) return String is
		-- return the value for the key in the object in string type
		use Ada.Strings;
		use KOW_Lib.Json;
		
		First_Dot : Integer;
	begin
		if Contains( Object, Key ) then
			return Get( Object, Key );
		end if;

		First_Dot := Fixed.Index( Key, ".", Forward );

		if First_Dot in Key'Range then
			declare
				Obj_Key : constant String := Key( Key'First .. First_Dot - 1 );
				New_Key : constant String := Key( First_Dot + 1 .. Key'Last );
			begin
				if Get_Type( Object, Obj_Key ) /= Json_Object then
					raise CONSTRAINT_ERROR with "Can't get key from a non object json data at " & key;
				end if;

				return Value_of( Get( Object, Obj_key ), New_key );
			end;
		else
			raise CONSTRAINT_ERROR with "unknown key: " & key;
		end if;
	end Value_Of;



	function Expand(
				Str	: in String;
				Object	: in KOW_Lib.Json.Object_Type
			) return String is
		-- expand the values ${..} in the string using the state
		function Value_of( Key : in String ) return String is
		begin
			return Value_Of( Object, Str );
		end Value_Of;

		function My_Expand is new KOW_Lib.String_Util.Expand( Value_of );
	begin
		return My_Expand( Str );
	end Expand;


	----------------
	-- Processors --
	----------------

	package body Processors is


		package Factory_Maps is new Ada.Containers.Hashed_Maps(
								Key_Type	=> Unbounded_String,
								Element_Type	=> Processors.Processor_Factory_Ptr,
								Hash		=> Ada.Strings.Unbounded.Hash,
								Equivalent_Keys	=> Ada.Strings.Unbounded."="
							);


		My_Factories : Factory_Maps.Map;

		----------------------------
		-- The Processor Registry --
		----------------------------

		function Get( Tag : in String ) return Processor_Interface'Class is
			-- return a processor for the given tag
			-- reutrn process_child_nodes by default

			use Ada.Strings.Unbounded;
			UTag : constant Unbounded_String := To_Unbounded_String( Tag );
		begin
			if Factory_Maps.Contains( My_Factories, UTag ) then
				return New_Processor( Factory_Maps.Element( My_Factories, UTag ).all );
			else
				return Defaults.New_Processor( Defaults.Factory );
			end if;
		end Get;


		procedure Set_Factory(
					Tag	: in String;
					Factory	: in Processor_Factory_Ptr
				) is
			use Ada.Strings.Unbounded;
			UTag : constant Unbounded_String := To_Unbounded_String( Tag );
		begin
			Factory_Maps.Include( My_Factories, UTag, Factory );
		end Set_Factory;



		package body Defaults is
			overriding
			procedure Process_Node(
						Processor	: in out Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
				-- call process_child_nodes
			begin
				Process_Child_Nodes( Processor, Doc, N, State );
				Process_Node_Attributes( Processor, Doc, N, State );
			end Process_Node;

			procedure Process_Child_Nodes(
						Processor	: in out Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
			
				use DOM.Core;

				List : Node_List;
				Child : Node;
			begin
				if Nodes.Has_Child_Nodes( N ) then
					List := Nodes.Child_Nodes( N );
					for i in 0 .. Nodes.Length( List ) - 1 loop
						Child := Nodes.Item( List, i );
						Process_Node(
								doc	=> doc,
								N	=> Child,
								State	=> State
							);
					end loop;
				end if;
			end Process_Child_Nodes;


			procedure Process_Node_Attributes(
						Processor	: in out Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
				-- process the node attributes, replacing the ${...} declarations
				use DOM.Core;
				Atts : Named_Node_Map := Nodes.Attributes( N );
				Att  : Node;
			begin
				for i in 0 .. Nodes.Length( Atts ) - 1 loop
					Att := Nodes.Item( Atts, i );
					Attrs.Set_Value( Att, Attrs.Value( Att ) );
				end loop;
			end Process_Node_Attributes;

			-----------------
			-- the factory --
			-----------------

			overriding
			function New_Processor(
						Factory	: in Factory_Type
					) return Processor_Interface'Class is
			begin
				return Processor_Type'( others => <> );
			end New_Processor;
		end Defaults;

		package body Generic_Factories is

			overriding
			function New_Processor(
					Factory	: in Factory_Type
				) return Processor_Interface'Class is
				-- allocate a uninitialized processor_Type, returning it
				Processor : Processor_Type;
			begin
				return Processor;
			end New_Processor;
		begin
			KOW_View.KTML.Processors.Set_Factory( Tag, Factory'Access );
		end Generic_Factories;

		package Implementations is
			package Each_Factories is new Generic_Factories( Each_Processor_Type, "kv:each" );
			-- <kv:each source="key_for_an_array_or_object" key="name_for_the_key" target="name_for_the_value" tag="span">


			overriding
			procedure Process_Node(
						Processor	: in out Each_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
				use DOM.Core;
				use KOW_Lib.Json;
				Local_State	: Json_Object_Type := State;
				Collection	: Json_Data_Type := Get( State, Elements.Get_Attribute( N, "source" ) );

				Key_Att		: constant String := Elements.Get_Attribute( N, "key" );
				Target_Att	: constant String := Elements.Get_Attribute( N, "target" );

				Template	: Node;

				procedure Append_Node is
					-- process the child nodes for the cloned child nodes; and append it to N.
					New_Child : Node := Nodes.Clone_Node( Template, True );
				begin
					New_Child := Nodes.Append_Child( N => N, New_Child => New_Child );
					Process_Child_Nodes(
								Processor	=> Each_Processor_Type'Class( Processor ),
								Doc		=> Doc,
								N		=> New_Child,
								State		=> Local_State
							);
				end Append_Node;

				procedure Object_Iterator( Key : in String; Value : in Json_Data_type ) is
				begin
					Set( Local_State, Key_Att, Key );
					Set( Local_State, Value_Att, Value );
					Append_Node;
				end Object_Iterator;
			begin
				-- Prepare the template and the container

				----------------------------
				-- iterate the collection --
				----------------------------
				case Type_Of( Collection ) is
					when Json_Object =>
						Iterate( Object => From_Data( Collection ), Iterator => Object_Iterator'Access );
					when Json_Array =>
						Iterate( A => From_Data( Collection ), iterator => Array_Iterator'Access );
					when others =>
						raise CONSTRAINT_ERROR with "Source for looping not array nor object";
				end case;
			end Process_Node;


	end Processors;

end KOW_View.KTML;

