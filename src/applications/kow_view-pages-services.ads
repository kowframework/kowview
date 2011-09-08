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
pragma License( GPL );


------------------------------------------------------------------------------
-- Main package for the Pages services                                      --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Directories;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_Lib.Ustring_Vectors;
with KOW_View.Components;
with KOW_View.Pages.Components;
with KOW_View.Services;
with KOW_View.Services.Implementations;
with KOW_View.Services.Singleton_Service_Cycles;
with KOW_View.Themes;


---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;


package KOW_View.Pages.Services is


	-------------------------------
	-- Helper Types and Packages --
	-------------------------------

	type Complete_Module_Type is record
		Module	: KOW_View.Components.Module_Ptr;
		Factory	: KOW_View.Components.Module_Factory_Ptr;
		Config	: KOW_Config.Config_File;
	end record;

	type Complete_Module_Array is array( Positive range <> ) of Complete_Module_Type;

	type Index_Array is array( Natural range <> ) of Positive;

	----------------------
	-- The Page Service --
	----------------------

	type Page_Service is new KOW_View.Services.Service_Type with record
		Title	: Unbounded_String;
		Author	: Unbounded_String;
	end record;


	overriding
	procedure Process_Json_Request(
			Service	: in out Page_Service;
			Request	: in     AWS.Status.Data;
			Response:    out KOW_Lib.Json.Object_Type
		);
	-- run initialize for each one of the modules in the page
	-- then call Process_Json_Request for a given module or group of modules.

	overriding
	procedure Process_Custom_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		);
	-- process the entire module cycle returning a HTML page


	procedure Process_Custom_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data;
			Page		: in     String;
			Initialize_Only	: in     Boolean
		);
	-- this is the actual procedure for processing the page and is here for
	-- the navigation component
	-- if initialize_only = true don't process head, body or foot...
	-- only tries to run initialize and then finalize..
	--
	-- IF any exception is raised during any of the procedures, simply jump to destroy
	-- all modules..

	function Get_Page(
				Service	: in Page_Service;
				Request	: in AWS.Status.Data
			) return String;
	-- retrieve the page name :)


	procedure Setup(
				Service	: in out Page_Service;
				Config	: in out KOW_Config.Config_File
			);


	package Page_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
						Service_Type	=> Page_Service,
						Component	=> KOW_View.Pages.Components.Component'Access
					);

	------------------------
	-- The Static Service --
	------------------------

	type Static_Service is new KOW_View.Services.Implementations.Resource_Service with null record;
	package Static_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
						Service_Type	=> Static_Service,
						Component	=> KOW_View.Pages.Components.Component'Access
					);



	--------------------------------------
	-- The Module Resource Service Type --
	--------------------------------------


	type Component_Resource_Service_Type is abstract new KOW_View.Services.Implementations.Resource_Service with null record;
	-- the type is abstract as it shouldn't be instanciated

	overriding
	function Locate_Resource(
			Service		: in Component_Resource_Service_Type;
			Resource	: in String;
			Extension	: in String := "";
			Virtual_Host	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Get_Default_Locale
		) return String;
	-- locate resource given:
	-- 	when resource is a URN using:
	-- 		component:component/somefile
	-- use the locate resource implementation for the given component, prefixing the resource by get_name(service)
	--
	-- or else use the locate resource for the current component



	----------------
	-- JS Service --
	----------------

	type JS_Service is new Component_Resource_Service_Type with null record;
	package JS_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
						Service_Type	=> JS_Service,
						Component	=> KOW_View.Pages.Components.Component'Access
					);
	
	-----------------
	-- CSS Service --
	-----------------

	type CSS_Service is new Component_Resource_Service_Type with null record;
	package CSS_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
						Service_Type	=> CSS_Service,
						Component	=> KOW_View.Pages.Components.Component'Access
					);

	--------------------
	-- Images Service --
	--------------------

	type Images_Service is new Component_Resource_Service_Type with null record;
	package Images_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
						Service_Type	=> Images_Service,
						Component	=> KOW_View.Pages.Components.Component'Access
					);


	-----------------
	-- Dir Service --
	-----------------

	type Dir_Service is new KOW_View.Services.Service_Type with null record;
	-- list all the available modules and services for every component registered


	overriding
	procedure Process_Json_Request(
			Service	: in out Dir_Service;
			Request	: in     AWS.Status.Data;
			Response:    out KOW_Lib.Json.Object_Type
		);

	overriding
	procedure Process_Custom_Request(
			Service		: in out Dir_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		);

	package Dir_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
						Service_Type	=> Dir_Service,
						Component	=> KOW_View.Pages.Components.Component'Access
					);

private

	Page_Script_Includes	: KOW_Lib.UString_Vectors.Vector;
	-- initialized by the body elaboration, includes all the script includes required by the
	-- page service except for dojo main package (as it requires configuration in HTML part)
	Page_Dojo_Packages	: KOW_Lib.UString_Vectors.Vector;
	-- same as Page_Script_Includes but for dojo packages
end KOW_View.Pages.Services;
