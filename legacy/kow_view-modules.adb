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

with ada.text_io;

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
with KOW_View.Util;


---------
-- AWS --
---------
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


	overriding
	function Get_Script_Includes(
			Module		: in     Module_Type
		) return KOW_Lib.UString_Vectors.Vector is 
	begin
		return Module.Script_Includes;
	end Get_Script_Includes;

	overriding
	function Get_Amdjs_Packages(
			Module		: in     Module_Type
		) return KOW_Lib.UString_Vectors.Vector is
	begin
		return Module.Amdjs_Packages;
	end Get_Amdjs_Packages;

	overriding
	function Get_Amdjs_CSS(
			Module		: in Module_Type
		) return KOW_Lib.UString_Vectors.Vector is
	begin
		return Module.Amdjs_CSS;
	end Get_Amdjs_CSS;


	overriding
	function Get_CSS_Includes(
			Module		: in     Module_Type
		) return KOW_Lib.UString_Vectors.Vector is
	begin
		return Module.CSS_Includes;
	end Get_CSS_Includes;


	procedure Include_Component_Script(
			Module		: in out Module_Type;
			Script		: in     String
		) is
	begin
		Include_Component_Script(
				Module		=> Module,
				Component	=> Module.Component.all,
				Script		=> Script
			);
	end Include_Component_Script;

	procedure Include_Component_Script(
			Module		: in out Module_Type;
			Component	: in     KOW_View.Components.Component_Type'Class;
			Script		: in     String
		) is
		Script_Path : Unbounded_String := To_Unbounded_String( "/pages/js/component:" );
	begin
		Append( Script_Path, To_String( KOW_View.Components.Get_Name( Component ) ) );
		Append( Script_Path, '/' );
		Append( Script_Path, Script );

		KOW_Lib.UString_Vectors.Append( Module.Script_Includes, Script_Path );
	end Include_Component_Script;

	procedure Include_Amdjs_Package(
			Module		: in out Module_Type;
			Amdjs_Package	: in     String
		) is
	begin
		KOW_Lib.UString_Vectors.Append( Module.Amdjs_Packages, To_Unbounded_String( Amdjs_Package ) );
	end Include_Amdjs_Package;


	procedure Include_Amdjs_CSS(
			Module		: in out Module_Type;
			Amdjs_CSS	: in     String
		) is
	begin
		KOW_Lib.UString_Vectors.Append( Module.Amdjs_CSS, To_Unbounded_String( Amdjs_CSS ) );
	end Include_Amdjs_CSS;


	procedure Include_Component_CSS(
			Module		: in out Module_Type;
			CSS		: in     String
		) is
	begin
		Include_Component_CSS(
				Module		=> Module,
				Component	=> Module.Component.all,
				CSS		=> CSS
			);
	end Include_Component_CSS;

	procedure Include_Component_CSS(
			Module		: in out Module_Type;
			Component	: in     KOW_View.Components.Component_Type'Class;
			CSS		: in     String
		) is
		CSS_Path : Unbounded_String := To_Unbounded_String( "/pages/css/component:" );
	begin	Append( CSS_Path, To_String( KOW_View.Components.Get_Name( Component ) ) );
		Append( CSS_Path, '/' );
		Append( CSS_Path, CSS );

		KOW_Lib.UString_Vectors.Append( Module.CSS_Includes, CSS_Path );
	end Include_Component_CSS;


	function Locate_Resource(
			Module		: in Module_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return String is
		use KOW_Lib.File_System;

		Prefix : constant String := To_String( Get_Name( Module ) ) & "_module";
	begin
		return Locate_Resource(
					Component	=> Module.Component.all,
					Resource	=> Prefix / Resource,
					Extension	=> Extension,
					Virtual_Host	=> Module.Virtual_Host,
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
			Locale			: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return String is
		-- helper method for calling templates parser's parse method and locate_resource

		use Templates_Parser;
		
		Resource 	: constant String := Locate_Resource(
						Module		=> Module_Type'Class( Module ),
						Resource	=> Template_Resource,
						Extension	=> Template_Extension,
						Kind		=> Ada.Directories.Ordinary_File,
						Locale		=> Locale
					);
		My_Parameters	: Translate_Set := Parameters;
	begin
		Insert( My_Parameters, Assoc( "module_id", Get_ID( Module ) ) );
		return Templates_Parser.Parse( Resource, My_Parameters );
	end Parse_Template;


	function Parse_Template(
			Module			: in Module_Type;
			Template_Resource	: in String;
			Template_Extension	: in String := "";
			Parameters		: in Templates_Parser.Translate_Set;
			Locale			: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return Unbounded_String is
		-- helper method for calling templates parser's parse method and locate_resource

		use Templates_Parser;
		
		Resource	: constant String := Locate_Resource(
						Module		=> Module_Type'Class( Module ),
						Resource	=> Template_Resource,
						Extension	=> Template_Extension,
						Kind		=> Ada.Directories.Ordinary_File,
						Locale		=> Locale
					);
		My_Parameters	: Translate_Set := Parameters;
	begin
		Insert( My_Parameters, Assoc( "module_id", Get_ID( Module ) ) );
		return Templates_Parser.Parse( Resource, My_Parameters );
	end Parse_Template;



	function Get_Name( Module : in Module_Type'Class ) return Module_Name is
	begin
		return KOW_View.Util.Get_Type_Name( Module_Tag, "_module" );
	end Get_Name;

end KOW_View.Modules;
