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
-- Component utility functions                                              --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;

package KOW_View.Components.Util is

	function Get_Name( Component_Tag : in Ada.Tags.Tag ) return String;
	-- get the name by the tag
	

	function Get_Name( URI : in String ) return String;
	-- get the component name for a given URI
	-- assumes URI is not empty nor  /

	function Locate_Resource(
			Component_Name	: in String;
			Resource	: in String;
			Extension	: in String;
			Kind		: in Ada.Directories.File_Kind	
		) return String;
	-- locate a resource file for this component
	-- this file should be placed at
	-- 	[WORKING_DIR]/data/kowview/component_name/resource.extension
	-- 	or
	-- 	[WORKING_DIR]/applications/component_name/data/resource.extension
	-- returning it's name if nothing has been found raise Ada.Directories.Name_Error if not found
	-- TODO: Implement locale support at Locate_Resource function

	function Load_Main_Configuration(
			Component_Name	: in String
		) return KOW_Config.Config_File;
	-- load the main configuration for this component
	
	function Load_Configuration(
			Component_Name		: in String;
			Configuration_Name	: in String
		) return KOW_Config.Config_File;
	-- load a configuration file from this component's relative path

end KOW_View.Components.Util;
