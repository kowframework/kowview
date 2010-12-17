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
with ada.text_io;			use ada.text_io;

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

package body KOW_View.Themes is
	

	function Template_Factory(
			Name	: in String;
			Config	: in KOW_Config.Config_File )
		return Template_Descriptor_Type is
		-- private method for loading the template descriptor from it's configuration
		Descriptor : Template_Descriptor_Type;
	begin
		Descriptor.Name		:= To_Unbounded_String( Name );
		Descriptor.Description	:= KOW_Config.Element( Config, "description" );

		Descriptor.Regions	:= KOW_Lib.String_Util.Explode(
							',',
							To_String(
								KOW_Config.Element( Config, "regions" )
							)
						);
		return Descriptor;
	end Template_Factory;


	
	function Theme_Factory(
			Name	: in String;
			Config	: in KOW_Config.Config_File )
		return Theme_Descriptor_Type is
		-- private method for loading the theme descriptor from it's configuration
		Descriptor : Theme_Descriptor_Type;
	begin
		Descriptor.Name			:= To_Unbounded_String( Name );
		Descriptor.Author		:= KOW_Config.Element( Config, "author" );
		Descriptor.Creation_Date	:= KOW_Config.Element( Config, "creation_time" );

		return Descriptor;
	end Theme_Factory;


	---------------
	-- Component --
	---------------



	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		) is
		-- Initialize the Theme component, setting every variable required
		use KOW_Config;
	begin
		Component.Default_Theme_Name	:= Value( Config, "default_theme", "default" );
		Component.Template_Extension	:= Value( Config, "template_extension", "html" );
		
		Templates_Registry.Factory_Registry.Register(
				"template",
				Template_Factory'Access
			);
		Templates_Registry.Reload_Registry;

		Themes_Registry.Factory_Registry.Register(
				"theme",
				Theme_Factory'Access
			);

		Themes_Registry.Reload_Registry;
	end Initialize;







end KOW_View.Themes;
