------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
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


-- The KTML is a superset of the XHTML language. Actually, it's just a
-- smart way to deal with XML files.

package KOW_View.KTML is



	-------------------
	--- Main Methods --
	-------------------


	function Render(
				File_Path	: in String;
				Initial_State	: in KOW_Lib.Json.Object_Type
			) return String;
	-- render the given file into a string

	
	procedure Process_Node(
				Doc	: in     DOM.Core.Document;
				N	: in out DOM.Core.Node;
				State	: in out KOW_Lib.Json.Object_Type
			);
	-- query the processor the given tag and process the node



	procedure Process_Child_Nodes(
				Doc	: in     DOM.Core.Document;
				N	: in out DOM.Core.Node;
				State	: in out KOW_Lib.Json.Object_Type
			);
	-- call process node for every child of the given node



	type Node_Processor_Access is access procedure(
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);


	---------------------------
	-- Processors Collection --
	---------------------------

	function Get_Processor( Tag : in String ) return Node_Processor_Access;
	-- return a processor for the given tag
	-- reutrn process_child_nodes by default


	procedure Set_Processor(
				Tag		: in String;
				Processor	: not null access procedure(
										Doc	: in     DOM.Core.Document;
										N	: in out DOM.Core.Node;
										State	: in out KOW_Lib.Json.Object_Type
									)
						);


private

	package Processor_Maps is new Ada.Containers.Hashed_Maps(
							Key_Type	=> Unbounded_String,
							Element_Type	=> Node_Processor_Access,
							Hash		=> Ada.Strings.Unbounded.Hash,
							Equivalent_Keys	=> Ada.Strings.Unbounded."="
						);


	My_Processors : Processor_Maps.Map;
end KOW_View.KTML;

