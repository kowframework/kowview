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


---------
-- Ada --
---------
with Ada.Calendar;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;
with KOW_View.Components.Registry;

---------
-- AWS --
---------

with AWS.MIME;
with AWS.Response;
with AWS.Session;
with AWS.Status;
with Templates_Parser;			use Templates_Parser;

package body KOW_View.Themes.Components is
	


	---------------
	-- Component --
	---------------



	overriding
	procedure Setup(
			Component	: in out Themes_Component;
			Config		: in out KOW_Config.Config_File
		) is
	-- setup the theme variables


		use KOW_Config;
	begin
		Component.Default_Theme_Name	:= Value( Config, "default_theme", "default" );
		Component.Template_Extension	:= Value( Config, "template_extension", "tpl" );
	end Setup;

	function Get_Theme_Name(
			Component	: in Themes_Component;
			Request		: in AWS.Status.Data
		) return String is
		Session_ID	: constant AWS.Session.ID := AWS.Status.Session (Request);
		User_Theme	: constant string := AWS.Session.Get( Session_ID, theme_name_session_key );
	begin
		if User_Theme /= "" then
			return User_Theme;
		else
			return To_String( Component.Default_Theme_Name );
		end if;
	end Get_Theme_Name;


end KOW_View.Themes.Components;
