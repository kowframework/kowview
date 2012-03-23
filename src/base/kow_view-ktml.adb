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
	begin
		if doc.Node_Type = Element_Node then
			Get_Processor(
					Elements.Get_Tag_Name( N )
				).all( Doc, N, State );
		else
			-- TODO :: replace the ${} to what we really want
			null;
		end if;
	end Process_Node;



	procedure Process_Child_Nodes(
				Doc	: in     DOM.Core.Document;
				N	: in out DOM.Core.Node;
				State	: in out KOW_Lib.Json.Object_Type
			) is
		-- call process node for each one of the children of the given node

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



	--------------------------
	-- Processor Collection --
	--------------------------


	function Get_Processor( Tag : in String ) return Node_Processor_Access is
		-- return a processor for the given tag
		-- the default processor only iterates in the childs
		use Ada.Strings.Unbounded;
		UTag : constant Unbounded_String := To_Unbounded_String( Tag );
	begin
		if Processor_Maps.Contains( My_Processors, UTag ) then
			return Processor_Maps.Element( My_processors, UTag );
		else
			return Process_Child_Nodes'Access;
		end if;
	end Get_Processor;

	procedure Set_Processor(
		Tag		: in String;
		Processor	: not null access procedure(
						Doc	: in     DOM.Core.Document;
						N	: in out DOM.Core.Node;
						State	: in out KOW_Lib.Json.Object_Type
					)
			) is
	begin
		Processor_Maps.Include(
					My_Processors,
					Ada.Strings.Unbounded.To_Unbounded_String( Tag ),
					null -- TODO :: Node_Processor_Access( Processor )
				);
	end Set_Processor;

end KOW_View.KTML;

