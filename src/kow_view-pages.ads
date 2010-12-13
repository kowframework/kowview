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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------


---------
-- Ada --
---------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Themes;

---------
-- AWS --
---------

with AWS.Response;
with AWS.Status;
with Templates_Parser;



package KOW_View.Pages is


	PAGE_CONFIG_ERROR : Exception;
	-- TODO: use this exception all over (overriding constraint_error whenever possible)



	function Load_Page_Config( Config_Name : in String ) return KOW_Config.Config_File;

	----------------
	-- Components --
	----------------

	type Component_Type is new KOW_View.Components.Component_Type with private;
	-- This component is responsible for calling all other components in order
	-- to render the page.
	--
	-- It's also responsible for calling the Theme component.
	


	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		);
	-- the only thing to setup is the theme_component


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return Module_Type'Class;
	-- Available modules:
	-- 	. page



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Type'Class;
	-- Available services:
	-- 	. page
	-- 	. static



	-------------
	-- Modules --
	-------------



	--
	-- Page Module
	--


	type Page_Module is new Module_Type with private;
	-- Responsible for managing the page loading.
	-- It's the engine for the Page_Service service.
	-- Can also be used in your own components and pages.

	overriding
	procedure Initialize_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		);
	-- this is where the page is initialized.


	procedure Initialize_Request(
			Module			: in out Page_Module;
			Request			: in     AWS.Status.Data;
			Parameters		: in out Templates_Parser.Translate_Set;
			Response		: in out AWS.Response.Data;
			Is_Final		:    out Boolean;
			Initialize_Modules_Only	: in     Boolean
		);
	-- where the page is really initialized.
	-- if initialize_modules_only is called, only initialize the modules


	overriding
	procedure Process_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- it's where the page is assembled.

	overriding
	procedure Finalize_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
		);

	--
	-- Static Module
	--
	
	type Static_Module is new Module_Type with private;

	overriding
	procedure Process_Request(
			Module		: in out Static_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- simply get some content and input inside the page;

	--
	-- Void Module
	--

	type Void_Module is new Module_Type with null record;
	-- this module does nothing.. it's only to reserve slots in the page config


	--------------
	-- Services --
	--------------


	--
	-- Page Service
	--


	type Page_Service is new Service_Type with private;
	-- a service usually represents a module to the external world.
	-- the service can be mapped to a base URI
	--      . when mapped to /do, /do/something will call it


	overriding
	procedure Process_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- This service provides direct access to the page module.

	--
	-- Static Service
	-- 

	
	type Static_Service is new Service_Type with private;

	overriding
	procedure Process_Request(
			Service		: in out Static_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- This service acts like a standard web server, providing access
	-- to static files.
private


	type Component_Type is new KOW_View.Components.Component_Type with record
		Theme_Component_Name: Unbounded_String;
	end record;


	type Page_Module is new Module_Type with record
		Config			: KOW_Config.Config_File;
		Processor		: KOW_View.Themes.Template_Processor_Module;
		-- there is no processing of the config file before 
		-- the page rendering begins.
		Theme_Component_Name	: Unbounded_String;
	end record;

	type Static_Module is new Module_Type with record
		Resource		: Unbounded_String;
		-- the resource (without extension) for the file to be queried
		-- we use resources in here to allow extending this module to be multilingual

		Extension	: Unbounded_String;
		-- the extension for the file (default is html)
	end record;

	type Page_Service is new Service_Type with record
		Mapping: Unbounded_String;
	end record;

	type Static_Service is new Service_Type with record
		Mapping: Unbounded_String;
	end record;

end KOW_View.Pages;
