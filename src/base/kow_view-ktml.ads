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
with Ada.Strings;
with Ada.Strings.Fixed;

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


-- TODO: kv:if
-- TODO: kv:case
-- TODO: kv:if_set
-- TODO: kv:region (this one will require some sort of
--
-- TODO: kv:extends/kv:extension_point => maybe it's not really needed

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



	--------------------
	-- Helper Methods --
	--------------------


	function Value_Of(
				Object	: in KOW_Lib.Json.Object_Type;
				Key	: in String
			) return String;
	-- return the value for the key in the object in string type

	function Expand(
				Str	: in String;
				Object	: in KOW_Lib.Json.Object_Type
			) return String;
	-- expand the values ${..} in the string using the state

	----------------
	-- Processors --
	----------------



	package Processors is
		-- this nested package is the heart of the KTML implementation.
		--
		-- A processor implements the Processor_Interface, which only changes the
		-- contents of a given node.

		-----------------------------
		-- The Processor Interface --
		-----------------------------

		type Processor_Interface is interface;
		
		procedure Process_Node(
					Processor	: in out Processor_Interface;
					Doc		: in     DOM.Core.Document;
					N		: in out DOM.Core.Node;
					State		: in out KOW_Lib.Json.Object_Type
				) is abstract;



		---------------------------
		-- The Processor Factory --
		---------------------------


		type Processor_Factory_Interface is interface;
		-- singleton object responsible for allocating processor_interfaces.


		type Processor_Factory_Ptr is access all Processor_Factory_Interface'Class;

		function New_Processor(
					Factory : in Processor_Factory_Interface
				) return Processor_Interface'Class is abstract;


		----------------------------
		-- The Processor Registry --
		----------------------------

		function Get( Tag : in String ) return Processor_Interface'Class;
		-- return a processor for the given tag
		-- reutrn process_child_nodes by default


		procedure Set_Factory(
					Tag	: in String;
					Factory	: in Processor_Factory_Ptr
				);



		package Defaults is
			type Processor_Type is new Processor_Interface with null record;
			-- call KOW_View.KTML.Process_Node for each one of the child elements

			overriding
			procedure Process_Node(
						Processor	: in out Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);
			-- call process_child_nodes


			procedure Process_Child_Nodes(
						Processor	: in out Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);


			procedure Process_Node_Attributes(
						Processor	: in out Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);
			-- process the node attributes, replacing the ${...} declarations
			

			-----------------
			-- the factory --
			-----------------

			type Factory_Type is new Processor_Factory_Interface with null record;

			overriding
			function New_Processor(
						Factory	: in Factory_Type
					) return Processor_Interface'Class;

			Factory : Factory_Type;
		end Defaults;


		generic
			type Processor_Type is new Processor_Interface with private;
			Tag : String;
		package Generic_Factories is
			type Factory_Type is new Processor_Factory_Interface with null record;

			overriding
			function New_Processor(
					Factory	: in Factory_Type
				) return Processor_Interface'Class;
			-- allocate a uninitialized processor_Type, returning it

			Factory : aliased Factory_Type;
			-- the singleton instance
		end Generic_Factories;



		---------------------
		-- Implementations --
		---------------------


		package Implementations is


			-----------------------------
			-- Iterable Processor Type --
			-----------------------------

			type Iterable_Processor_Type is abstract new Defaults.Processor_Type with record
				Item_Templates		: DOM.Core.Node_List;
				Next_Item		: Natural := 0;
				Empty_Template		: DOM.Core.Node;
				Is_Empty		: Boolean := True;
			end record;

			procedure Initialize_Templates(
						Processor	: in out Iterable_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in     DOM.Core.Node
					);

			procedure Create_Item(
						Processor	: in out Iterable_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);
			-- create a new item using one of the Item Templates


			procedure Create_Empty_If_Needed(
						Processor	: in out Iterable_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);
			-- create the item representing empty iterators if needed (ie, no call to create_item has been made and
			-- there is the <kv:empty> child element in this iterator parent node

			-------------------------
			-- Each Processor Type --
			-------------------------

			type Each_Processor_Type is new Iterable_Processor_Type with null record;
			-- <kv:each source="key_for_an_array_or_object" key="key" target="item" tag="ul" reverse="false">
			-- 	<kv:item tag="li">${name_for_the_value} is named ${key}</kv:item>
			-- 	<kv:empty>There is no item in here brutha!</kv:empty>
			-- </kv:each>


			overriding
			procedure Process_Node(
						Processor	: in out Each_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);


			------------------------
			-- For Processor Type --
			------------------------

			type For_Processor_Type is new Iterable_Processor_Type with null record;
			-- <kv:for from="1" to="10" target="item" tag="ol" reverse="false">

			overriding
			procedure Process_Node(
						Processor	: in out For_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);


			------------------------
			-- Set Processor Type --
			------------------------

			type Set_Processor_Type is new Processor_Interface with null record;
			-- <kv:set key="theKey" value="theValue"/>

			overriding
			procedure Process_Node(
						Processor	: in out Set_Processor_Type;
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);
		end Implementations;
	end Processors;

end KOW_View.KTML;

