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
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;


-------------
-- XML/Ada --
-------------
with DOM.Core;
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
		else
			-- TODO :: replace the ${} to what we really want
			null;
		end if;
	end Process_Node;



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
					for i in 0 .. Nodes.Length( List ) loop
						Child := Nodes.Item( List, i );
						Process_Node(
								doc	=> doc,
								N	=> Child,
								State	=> State
							);
					end loop;
				end if;
			end Process_Child_Nodes;

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
	end Processors;




end KOW_View.KTML;

