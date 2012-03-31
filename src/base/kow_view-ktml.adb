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
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Tags;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_Lib.String_Util;
with KOW_View.DOM_Util;


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
		KOW_Lib.Log.Log( Logger, KOW_Lib.Log.Level_Info, "About to parse " & File_Path );

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

	begin
		if N.Node_Type = Element_Node then
			declare
				Tag : constant String := Elements.Get_Tag_Name( N );
				Processor : Processors.Processor_Interface'Class := Processors.Get( Tag );
			begin
				KOW_Lib.Log.Log( Logger, KOW_Lib.Log.Level_Info, "parsing element node of tag " & Tag & " using " & Ada.Tags.Expanded_name( Processor'Tag ) );
				Processors.Process_Node(
							Processor	=> Processor,
						       	Doc		=> Doc,
							N		=> N,
    							State		=> State 
						);
			end;
		elsif N.Node_Type = Text_Node then
			KOW_Lib.Log.Log( Logger, KOW_Lib.Log.Level_Info, "parsing text node" );
			Nodes.Set_Node_Value( N, Expand( Nodes.Node_Value( N ), State ) );
		else
			KOW_Lib.Log.Log( Logger, KOW_Lib.Log.Level_Warning, "skipping node of type "  & Node_Types'Image( N.Node_Type ) );
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
		
		First_Dot	: Integer;
		Data		: Json_Data_Type;
	begin
		if Contains( Object, Key ) then
			Data := Get( Object, Key );
			return To_String( Data );
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
			raise CONSTRAINT_ERROR with "unknown key: """ & key & '"';
		end if;
	end Value_Of;



	function Expand(
				Str	: in String;
				Object	: in KOW_Lib.Json.Object_Type
			) return String is
		-- expand the values ${..} in the string using the state
		function Value_of( Key : in String ) return String is
		begin
			return Value_Of( Object, Key );
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
			KOW_Lib.Log.Log( Logger, KOW_Lib.Log.Level_Info, "registering processor for tag " & Tag );
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



		---------------------
		-- Implementations --
		---------------------


		package body Implementations is


			-----------------------------
			-- Iterable Processor Type --
			-----------------------------

			procedure Initialize_Templates(
						Processor	: in out Iterable_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in     DOM.Core.Node
					) is
				use DOM.Core;

				Items	   : Node_List := Elements.Get_Elements_By_Tag_Name( N, "kv:item" );
				Empties	   : Node_List := Elements.Get_Elements_By_Tag_Name( N, "kv:empty" );
				Container  : Node := Documents.Create_Element( doc, "kv:item_template_container" );
				EContainer : Node := Documents.Create_Element( doc, "kv:empty_template_container" );


				procedure New_Item_Template( Item_Node : Node ) is
					New_Item : Node := DOM_Util.Create_From_Template( Doc, Item_Node, "li", True );
				begin
					New_Item := Nodes.Append_Child( N => Container, New_Child => New_Item );
				end New_Item_Template;

				procedure New_Empty_Template( Empty_Node : Node ) is
				begin
					Processor.Empty_Template := DOM_Util.Create_From_Template( Doc, Empty_Node, "li", True );
					Processor.Empty_Template := Nodes.Append_Child( N => Container, New_Child => Processor.Empty_Template );
				end New_Empty_Template;
			begin

				-- append the containers so they'll be deallocaded when needed
				Container := Nodes.Append_Child( N => N, New_Child => Container );
				EContainer:= Nodes.Append_Child( N => N, New_Child => EContainer );

				-- Item Templates
				for i in 0 .. Nodes.Length( Items ) - 1 loop
					New_Item_Template( Nodes.Item( Items, i ) );
				end loop;

				if not Nodes.Has_Child_Nodes( Container ) then
					raise PROGRAM_ERROR with "you need to specify some kv:item elements in your loop";
				end if;
				Processor.Item_Templates := Nodes.Child_Nodes( Container );


				-- the empty template
				if Nodes.Length( Empties ) > 0 then
					New_Empty_Template( Nodes.Item( Empties, 0 ) );
				end if;

			end Initialize_Templates;



			procedure Get_Item_Template(
						Processor	: in out Iterable_Processor_Type;
						New_Node	:    out DOM.Core.Node
					) is
			begin
				New_Node := DOM.Core.Nodes.Clone_Node( DOM.Core.Nodes.Item( Processor.Item_Templates, Processor.Next_Item ), Deep => True );
				Processor.Next_Item := Processor.Next_Item + 1;

				if Processor.Next_Item >= DOM.Core.Nodes.Length( Processor.Item_Templates ) then
					Processor.Next_Item := 0;
				end if;
			end Get_Item_Template;

			procedure Create_Item(
						Processor	: in out Iterable_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
				-- create a new item using one of the Templates
				

				New_Child : DOM.Core.Node;
			begin
				Processor.Is_Empty := False;
				Get_Item_Template( Processor, New_Child );
				New_Child := DOM.Core.Nodes.Append_Child( N => N, New_Child => New_Child );
				KOW_View.KTML.Process_Node(
								Doc		=> Doc,
								N		=> New_Child,
								State		=> State
							);
			end Create_Item;

			procedure Create_Empty_If_Needed(
						Processor	: in out Iterable_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
				use DOM.Core;
				use DOM.Core.Nodes;
				New_Child : Node;
			begin
				if not Processor.Is_Empty or else Processor.Empty_Template = null then
					-- if there is no kv:empty block, does nothing
					return;
				end if;

				New_Child := Append_Child( N, Nodes.Clone_Node( Processor.Empty_Template, True ) );
				KOW_View.KTML.Process_Node(
								Doc	=> Doc,
								N	=> New_Child,
								State	=> State
							);
			end Create_Empty_If_Needed;



			-------------------------
			-- Each Processor Type --
			-------------------------


			package Each_Factories is new Generic_Factories( Each_Processor_Type, "kv:each" );


			overriding
			procedure Process_Node(
						Processor	: in out Each_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
				use DOM.Core;
				use KOW_Lib.Json;




				-- parameters:
				Collection	: Json_Data_Type := Get( State, Elements.Get_Attribute( N, "source" ) );
				Key_Str		: constant String := DOM_Util.Node_Attribute( N, "key", "key" );
				Target_Str	: constant String := DOM_Util.Node_Attribute( N, "target", "item" );
				Reverse_Str	: constant String := DOM_Util.Node_Attribute( N, "reverse", "false" );

				-- work variables:
				New_N		: Node := DOM_Util.Create_From_Template( Doc, N, "ul", False );
				Local_State	: Object_Type := State;


				function Reversed return Boolean is
				begin
					return Boolean'Value( Reverse_Str );
				exception
					when others =>
						raise CONSTRAINT_ERROR with "'" & Reverse_Str & "' is not a valid boolean value in reverse attribute";
				end Reversed;


				procedure Append_Node is
				begin
					Create_Item(
							Processor	=> Processor,
							Doc		=> Doc,
							N		=> New_N,
							State		=> Local_State
						);
				end Append_Node;


				procedure Object_Iterator( Key : in String; Value : in Json_Data_type ) is
				begin
					Set( Local_State, Key_Str, Key );
					Set( Local_State, Target_Str, Value );
					Append_Node;
				end Object_Iterator;


				procedure Array_Iterator( Index : in Natural; Value : in Json_Data_Type ) is
				begin
					Set( Local_State, Key_Str, Index );
					Set( Local_State, Target_Str, Value );
					Append_Node;
				end Array_Iterator;


			begin
				-- Prepare the template and the container


				Initialize_Templates(
						Processor	=> Processor,
						Doc		=> Doc,
						N		=> N
					);

				-- Setup the template
				Elements.Remove_Attribute( New_N, "source" );
				Elements.Remove_Attribute( New_N, "key" );
				Elements.Remove_Attribute( New_N, "target" );

				-- iterate the collection
				case Get_Type( Collection ) is
					when Json_Object =>
						if Reversed then
							Reverse_Iterate( Object => From_Data( Collection ), Iterator => Object_Iterator'Access );
						else
							Iterate( Object => From_Data( Collection ), Iterator => Object_Iterator'Access );
						end if;
					when Json_Array =>
						if Reversed then
							Reverse_Iterate( A => From_Data( Collection ), iterator => Array_Iterator'Access );
						else
							Iterate( A => From_Data( Collection ), iterator => Array_Iterator'Access );
						end if;
					when others =>
						raise CONSTRAINT_ERROR with "Source for looping not array nor object";
				end case;

				Create_Empty_If_Needed(
							Processor	=> Processor,
							Doc		=> Doc,
							N		=> New_N,
							State		=> State
					);


				-- replace and free the node
				N := Nodes.Replace_Child(
							N		=> Nodes.Parent_Node( N ),
							New_Child	=> New_N,
							Old_Child	=> N
						);
				Nodes.Free( N, True );
			end Process_Node;


			------------------------
			-- For Processor Type --
			------------------------

			package For_Factories is new Generic_Factories( For_Processor_Type, "kv:for" );

			overriding
			procedure Process_Node(
						Processor	: in out For_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
				use DOM.Core;
				From_Str	: constant String := Elements.Get_Attribute( N, "from" );
				To_Str		: constant String := Elements.Get_Attribute( N, "to" );
				Target_Str	: constant String := DOM_Util.Node_Attribute( N, "target", "item" );
				Reverse_Str	: constant String := DOM_Util.Node_Attribute( N, "reverse", "false" );
				New_N		: Node;
				Local_State	: KOW_Lib.Json.Object_Type := State;

				function From return Integer is
				begin
					return Integer'Value( From_Str );
				exception
					when others =>
						raise CONSTRAINT_ERROR with "'" & From_Str & "' is not a valid integer value in from attribute";
				end From;

				function To return Integer is
				begin
					return Integer'Value( To_Str );
				exception
					when others =>
						raise CONSTRAINT_ERROR with "'" & To_Str & "' is not a valid integer value in to attribute";
				end To;

				function Reversed return Boolean is
				begin
					return Boolean'Value( Reverse_Str );
				exception
					when others =>
						raise CONSTRAINT_ERROR with "'" & Reverse_Str & "' is not a valid boolean value in reverse attribute";
				end Reversed;



				procedure Append_Node( Item : Integer ) is
				begin
					KOW_Lib.Json.Set( Local_State, Target_Str, Item );
					Create_Item(
							Processor	=> Processor,
							Doc		=> Doc,
							N		=> New_N,
							State		=> Local_State
						);
				end Append_Node;

			begin
				Initialize_Templates(
							Processor	=> Processor,
							Doc		=> Doc,
							N		=> N
						);

				New_N := DOM_Util.Create_From_Template( Doc, N, "ol", False );

				Elements.Remove_Attribute( New_N, "from" );
				Elements.Remove_Attribute( New_N, "to" );
				Elements.Remove_Attribute( New_N, "target" );
				Elements.Remove_Attribute( New_N, "reverse" );

				if Reversed then
					for i in reverse From .. To loop
						Append_Node( i );
					end loop;
				else
					for i in From .. To loop
						Append_Node( i );
					end loop;
				end if;


				Create_Empty_If_Needed(
							Processor	=> Processor,
							Doc		=> Doc,
							N		=> New_N,
							State		=> State
						);

				N := Nodes.Replace_Child(
							N		=> Nodes.Parent_Node( N ),
							Old_Child	=> N,
							New_Child	=> New_N
						);

				Nodes.Free( N, True );

				N := New_N;
			end Process_Node;

			------------------------
			-- Set Processor Type --
			------------------------

			package Set_Factories is new Generic_Factories( Set_Processor_Type, "kv:set" );
			-- <kv:set key="theKey" value="theValue"/>

			overriding
			procedure Process_Node(
						Processor	: in out Set_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					) is
				Key_Str		: constant String := DOM.Core.Elements.Get_Attribute( N, "key" );
				Value		: constant String := DOM.Core.Elements.Get_Attribute( N, "value" );


				procedure Set_State(
							Obj	: in out KOW_Lib.Json.Object_Type;
							Key	: in     String
						) is
					use Ada.Strings;
					use KOW_Lib.Json;
					Idx	: constant Integer := Fixed.Index( Key, ".", Forward );
				begin
					if Idx in Key'Range then
						declare
							Child		: Object_Type;
							Object_key	: constant String := Key( Key'First .. Idx - 1 );
							Property_Key	: constant String := Key( Idx + 1 .. Key'Last );
						begin
							if Contains( Obj, Object_Key ) then
								Child := Get( Obj, Object_Key );
							end if;
							Set_State( Child, Property_Key );
							Set( Obj, Object_Key, Child );
						end;
					else
						Set( Obj, Key, Value );
					end if;
				end Set_State;

			begin
				if Key_Str = "" then
					raise CONSTRAINT_ERROR with "you need to specify a key for your value";
				end if;


				Set_State( State, Key_Str );
			end Process_Node;


		end Implementations;
	end Processors;

end KOW_View.KTML;

