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
pragma License( GPL );




-------------------
-- KOW Framework --
-------------------
with KOW_Lib.String_Util;
with KOW_View.Components.Registry;
with KOW_View.Themes.Components;




package body KOW_View.Themes is
	--------------------
	-- Helper Methods --
	--------------------

	function Locate_Theme_Resource(
			Component_Name	: in String;
			Theme_Name	: in String;
			Resource	: in String;
			Extension	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Get_Default_Locale
		) return String is
	begin
		return KOW_View.Themes.Components.Locate_Resource(
				Component	=> KOW_View.Themes.Components.Component,
				Resource	=> Theme_Name & Separator & Resource,
				Extension	=> Extension,
				Kind		=> Kind,
				Locale		=> Locale
			);
	end Locate_Theme_Resource;




	----------------------------------------
	-- Theme Management and configuration --
	----------------------------------------

	------------
	-- Themes --
	------------

	function Theme_Factory(
				Name	: in String;
				Config	: in KOW_Config.Config_File
			) return Theme_Type is
		-- private method for loading the theme descriptor from it's configuration
		Descriptor : Theme_Type;
	begin
		Descriptor.Name			:= To_Unbounded_String( Name );
		Descriptor.Author		:= KOW_Config.Element( Config, "author" );
		Descriptor.Creation_Date	:= KOW_Config.Element( Config, "creation_time" );

		return Descriptor;
	end Theme_Factory;



	---------------
	-- Templates --
	---------------



	function Template_Factory(
				Name	: in String;
				Config	: in KOW_Config.Config_File
			) return Template_Type is
		-- private method for loading the template descriptor from it's configuration
		Descriptor : Template_Type;
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


	

end KOW_View.Themes;
