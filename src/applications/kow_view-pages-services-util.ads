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
-- Utility for handling page services                                       --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_View.Themes;

package KOW_View.Pages.Services.Util is
	function Get_Config_File( Page : in String ) return KOW_Config.Config_File;
	-- get the config file for the given page..

	function Get_Template( Config : in KOW_Config.Config_File ) return KOW_View.Themes.Template_Type;
	-- get the template for the given configuration file


	function Get_Modules( Config : in KOW_Config.Config_File ) return Complete_Module_Array;
	-- get all the complete_module_type types initializing the properties:
	-- 	factory
	-- 	region
	-- 	config
	-- in the order they are declared inside the configuration file.


	function Get_Module_IDs(
				Config	: in KOW_Config.Config_File;
				Region	: in KOW_View.Themes.Region_Type
			) return Index_Array;

private


	package Config_File_Maps is new Ada.Containers.Ordered_Maps(
				Key_Type	=> Unbounded_String,
				Element_Type	=> KOW_Config.Config_File,
				"="		=> KOW_Config."="
			);

	protected Page_Config_Cache is
		procedure Get_Config_File( Config : out KOW_Config.Config_File; Page : in String );
		-- check if the config file is in the map... if not, read it into the map
		-- return the config file if available
	private
		Cache : Config_File_Maps.Map;
	end Page_Config_Cache;


end KOW_View.Pages.Services.Util;
