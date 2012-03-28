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



	type Node_Processor_Access is access procedure(
						Doc		: in     DOM.Core.Document;
						N		: in out DOM.Core.Node;
						State		: in out KOW_Lib.Json.Object_Type
					);

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




		package Implementations is
			type Each_Processor_Type is new Defaults.Processor_Type with null record;

			type For_Processor_Type is new Defaults.Processor_Type with null record;
		end Implementations;
	end Processors;

end KOW_View.KTML;

