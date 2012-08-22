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
-- Component type is a tagged type responsible for handling resources.      --
--                                                                          --
-- Each component has one instance of the Component_Type or a subtype.      --
------------------------------------------------------------------------------


---------
-- Ada --
---------
with Ada.Directories;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_Lib.UString_Vectors;


---------
-- AWS --
---------
with AWS.Response;



package KOW_View.Components is

	--------------------
	-- Component Type --
	--------------------

	type Component_Type is tagged record
		Name	: Component_Name;
	end record;

	type Component_Access is not null access all Component_Type'Class;
	-- whenever possible use Component_Access as your pointer type
	type Component_Ptr is access all Component_Type'Class;


	function Locate_Resource(
			Component	: in Component_Type;
			Resource	: in String;
			Extension	: in String := "";
			Virtual_Host	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return String;
	-- locate a resource file for this component
	-- this file should be placed at
	-- 	[WORKING_DIR]/data/component_name/resource.extension
	-- 	or
	-- 	[WORKING_DIR]/components/component_name/resource.extension
	-- returning it's name if nothing has been found raise Ada.Directories.Name_Error if not found
	--
	-- Also, when Locale_Code is provided look for the resources in a localized way.
	-- 
	-- For instance, when looking for component's foo the bar.ext resource in pt_BR locale it'd look for:
	--
	-- 1. [WORKING_DIR]/data/foo/bar_pt_BR.ext
	-- 2. [WORKING_DIR]/data/foo/bar_pt.ext
	-- 3. [WORKING_DIR]/data/foo/bar.ext
	-- 4. [WORKING_DIR]/components/foo/bar_pt_BR.ext
	-- 5. [WORKING_DIR]/components/foo/bar_pt.ext
	-- 6. [WORKING_DIR]/components/foo/bar.ext
	--
	--
	-- If KOW_View.Enable_Virtual_Host = true and VIrtual_host /= "" then look for
	--    [WORKING_DIR]/vhosts/[VIRTUAL_HOST]/foo/bar_pt_BR.ext
	--    [WORKING_DIR]/vhosts/[VIRTUAL_HOST]/foo/bar_pt.ext
	--    [WORKING_DIR]/vhosts/[VIRTUAL_HOST]/foo/bar.ext
	-- before looking in the other locations
	--
	-- Returning the first file relative path found



	function Load_Config(
			Component	: in Component_Type;
			N		: in String
		) return KOW_Config.Config_File_Type;
	-- load the configuration file


	function New_Component(
				Name	: in String )
			) return Component_Ptr;
	-- allocate and initialize the component
	-- use this to declare your own components
end KOW_View.Components;
