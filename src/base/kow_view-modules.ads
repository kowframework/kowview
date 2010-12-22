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
-- Base package for modules in KOW View                                     --
--                                                                          --
-- A module is a isolated piece of information that can be embeded into a   --
-- web page.                                                                --
--                                                                          --
-- The module type should be suffixed _Module                               --
------------------------------------------------------------------------------





--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_View.Components;		use KOW_View.Components;


---------
-- AWS --
---------
with AWS.Status;

package KOW_View.Modules is

	
	-----------------
	-- Module Type --
	-----------------

	type Module_Type is abstract new KOW_View.Components.Module_Interface with record
		Context		: Unbounded_String;
		-- where it's bein created


		ID		: Positive;
		-- a number to identify the module in this request/page


		ID_Count	: Natural := 0;
		-- count all the ids that have been generated for this module

		Component	: Component_Access;
		-- the component that owns this module
	end record;


	overriding
	function Get_ID( Module : in Module_Type ) return Positive;


	
	function Locate_Resource(
			Module		: in Module_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Get_Default_Locale
		) return String;



	procedure Generate_HTML_ID(
				Module		: in out Module_Type;
				The_ID		:    out Unbounded_String
		);
	-- procedure used to generate a valid ID for HTML elements
	-- it's a helper procedure so the user can produce unique IDs for their pages easily



	function Get_Name( Module : in Module_Type'Class ) return String;

end KOW_View.Modules;
