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
-- Root package for theme handling.                                         --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Directories;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Generic_Registry;
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.Locales;
with KOW_Lib.UString_Vectors;


package KOW_View.Themes is
	--------------------
	-- Helper Methods --
	--------------------

	theme_name_session_key: constant string := "KOW_view::theme_name";

	function Locate_Theme_Resource(
			Component_Name	: in String;
			Theme_Name	: in String;
			Resource	: in String;
			Extension	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Get_Default_Locale
		) return String;




	------------
	-- Themes --
	------------

	type Theme_Type is record
		-- A record type for describing how a theme operates and other information.
		Name		: Unbounded_String;
		Author		: Unbounded_String;
		Creation_Date	: Unbounded_String; -- TODO: store the creation date as Ada.Calendar.Time
	end record;


	function Theme_Factory(
				Name	: in String;
				Config	: in KOW_Config.Config_File
			) return Theme_Type;


	package Themes_Registry is new KOW_Config.Generic_Registry(
				Element_Type	=> Theme_Type,
				Relative_Path	=> "kowview" / "themes" / "themes"
			);
	-- Store all the available theme's descriptor.


	
	---------------
	-- Templates --
	---------------


	type Region_Type is new Unbounded_String;


	type Template_Type is record
		-- A record type describing a template.
		-- This is used to describe the templates that are expected to be found in
		-- all themes.
		--
		-- Also, contains information about the author and why is this template required.
		Name		: Unbounded_String;
		Description	: Unbounded_String;
		Regions		: KOW_Lib.UString_Vectors.Vector;
	end record;

	function Template_Factory(
				Name	: in String;
				Config	: in KOW_Config.Config_File
			) return Template_Type;




	package Templates_Registry is new KOW_Config.Generic_Registry(
				Element_Type	=> Template_Type,
				Relative_Path	=> "kowview" / "themes" / "templates"
			);
	-- Store all required templates.



end KOW_View.Themes;
