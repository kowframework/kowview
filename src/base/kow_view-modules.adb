------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
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


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.File_System;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Modules.Util;


---------
-- AWS --
---------
with AWS.Status;
with Templates_Parser;


package body KOW_View.Modules is



	------------
	-- Module --
	------------

	overriding
	function Get_ID( Module : in Module_Type ) return Positive is
	begin
		return Module.ID;
	end Get_ID;

	function Locate_Resource(
			Module		: in Module_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Get_Default_Locale
		) return String is
		use KOW_Lib.File_System;

		Prefix : constant String := Get_Name( Module ) & "_module";
	begin
		return Locate_Resource(
					Component	=> Module.Component.all,
					Resource	=> Prefix / Resource,
					Extension	=> Extension,
					Kind		=> Kind,
					Locale		=> Locale
				);
	end Locate_Resource;



	procedure Generate_HTML_ID(
				Module		: in out Module_Type;
				The_ID		:    out Unbounded_String
		) is
		-- procedure used to generate a valid ID for HTML elements
		-- it's a helper procedure so the user can produce unique IDs for their pages easily

		function T( N : in Natural ) return String is
			use Ada.Strings, Ada.Strings.Fixed;
		begin
			return Trim( Natural'Image( N ), Both );
		end T;

	begin
		Module.ID_Count := Module.ID_Count + 1;

		The_ID := To_Unbounded_String( "module_" & T( Natural( Module.ID ) ) & "_id_" & T( Module.ID_Count ) );
				
	end Generate_HTML_ID;


	function Parse_Template(
			Module			: in Module_Type;
			Template_Resource	: in String;
			Template_Extension	: in String := "";
			Parameters		: in Templates_Parser.Translate_Set;
			Locale			: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Get_Default_Locale
		) return String is
		-- helper method for calling templates parser's parse method and locate_resource

		
		Resource : constant String := Locate_Resource(
						Module		=> Module_Type'Class( Module ),
						Resource	=> Template_Resource,
						Extension	=> Template_Extension,
						Kind		=> Ada.Directories.Ordinary_File,
						Locale		=> Locale
					);
	begin
		return Templates_Parser.Parse( Resource, Parameters );
	end Parse_Template;


	function Parse_Template(
			Module			: in Module_Type;
			Template_Resource	: in String;
			Template_Extension	: in String := "";
			Parameters		: in Templates_Parser.Translate_Set;
			Locale			: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Get_Default_Locale
		) return Unbounded_String is
		-- helper method for calling templates parser's parse method and locate_resource

		
		Resource : constant String := Locate_Resource(
						Module		=> Module_Type'Class( Module ),
						Resource	=> Template_Resource,
						Extension	=> Template_Extension,
						Kind		=> Ada.Directories.Ordinary_File,
						Locale		=> Locale
					);
	begin
		return Templates_Parser.Parse( Resource, Parameters );
	end Parse_Template;



	function Get_Name( Module : in Module_Type'Class ) return String is
	begin
		return KOW_View.Modules.Util.Get_Name( Module'Tag );
	end Get_Name;

end KOW_View.Modules;
