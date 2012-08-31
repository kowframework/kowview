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

--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;




-------------------
-- KOW Framework --
-------------------
with KOW_View.Components;	use KOW_View.Components;
with KOW_View.Module_Factories;
with KOW_View.Pages;		use KOW_View.Pages;



package KOW_View.Modules is


	-----------------
	-- Base Module --
	-----------------
	subtype Module_Name is Name_Type;

	type Base_Module( Component : Component_Ptr ) is abstract new Module_Interface with null record;
	-- a module to be used as base to other module implementations


	function Get_Name( Module : in Base_Module ) return Module_Name;
	-- giving the module type is name SOMETHING_Module, returns SOMETHING

	function Locate_Resource(
			Module		: in Base_Module;
			Status		: in Request_Status_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String;
	-- locate resource using the Component's locate_resource method
	-- the resource will be:
	-- [module_name]_module/resource



	-----------------
	-- KTML Module --
	-----------------

	type KTML_Module ( Component : Component_Ptr ) is abstract new Base_Module (Component) with null record;
	-- render a template using the results from Process_Json_Request
	-- the json procedure should determine if it's being called by the page ASYNC
	-- Json request handler or is rendering the HTML by the Status.Mode variable
	--
	-- the template name should be set by the process_json_request, using the template variable within the returning
	-- object
	-- will use the "success.ktml" by default.


	overriding
	procedure Process_Request(
				Module	: in out KTML_Module;
				Page	: in out Page_Interface'Class;
				Status	: in     Request_Status_Type
			);

	-------------------
	-- Static Module --
	-------------------



	generic
		TheComponent : Component_Ptr;
		Resource  : String;
		-- the resource to be served (to be located by module's Locate_Resource function)
		-- the "html" extension is always used
	package Static_Modules is

		-- instances of this package appends the given file as text
		-- in the page service

		-- TODO :: implement cache

		----------------
		-- The Module --
		----------------

		--type Static_Module is new Base_Module ( Component ) with null record;
		type Static_Module is new Module_Interface with record
			Component : KOW_View.Components.Component_Ptr := TheComponent;
		end record;

		overriding
		procedure Process_Request(
					Module	: in out Static_Module;
					Page	: in out Page_Interface'Class;
					Status	: in     Request_Status_Type
				);
		-- append the File_Path contents into the page
		
		overriding
		procedure Process_Json_Request(
					Module	: in out Static_Module;
					Page	: in out Page_interface'Class;
					Status	: in     Request_Status_Type;
					Response:    out KOW_Lib.Json.Object_Type
				);
		-- raises program_error with "can't serve static files in JSON requests"


		-----------------
		-- The Factory --
		-----------------

		package Factories is new Module_Factories.Singleton_Modules( Module_Type => Static_Module );
		
		Factory : constant Module_Factory_Ptr := new Factories.Singleton_Module_Factory;
	end Static_Modules;



end KOW_View.Modules;

