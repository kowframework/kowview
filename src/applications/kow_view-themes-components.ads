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


---------
-- Ada --
---------
with Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Generic_Registry;
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.Locales;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;		use KOW_View.Components;

---------
-- AWS --
---------

with AWS.Response;
with AWS.Status;
with Templates_Parser;

-- TODO: create module for theme listing
-- TODO: create service for theme selection

package KOW_View.Themes is


	---------------
	-- Component --
	---------------


	type Themes_Component is new KOW_View.Components.Component_Type with record
		Default_Theme_Name	: Unbounded_String; -- default
		Name			: Unbounded_String;
		Template_Extension	: Unbounded_String; -- html
	end record;

	Component : aliased Themes_Component;
	-- the only component instance

	

	overriding
	procedure Setup(
			Component	: in out Component_Type;
			Config		: in out KOW_Config.Config_File
		);
	-- setup the theme variables

	
	----------------------------------------
	-- Theme Management and configuration --
	----------------------------------------

	-- The following methods and types are for internal use only.
	--
	-- That's what runs KOW_View.Themes component.


	type Theme_Descriptor_Type is record
		-- A record type for describing how a theme operates and other information.
		Name		: Unbounded_String;
		Author		: Unbounded_String;
		Creation_Date	: Unbounded_String; -- TODO: store the creation date as Ada.Calendar.Time
	end record;
	

	type Template_Descriptor_Type is record
		-- A record type describing a template.
		-- This is used to describe the templates that are expected to be found in
		-- all themes.
		--
		-- Also, contains information about the author and why is this template required.
		Name		: Unbounded_String;
		Description	: Unbounded_String;
		Regions		: KOW_Lib.UString_Vectors.Vector;
	end record;

	package Themes_Registry is new KOW_Config.Generic_Registry(
				Element_Type	=> Theme_Descriptor_Type,
				Relative_Path	=> "kowview" & Separator & "themes" & Separator & "themes"
			);
	-- Store all the available theme's descriptor.


	package Templates_Registry is new KOW_Config.Generic_Registry(
				Element_Type	=> Template_Descriptor_Type,
				Relative_Path	=> "kowview" & Separator & "themes" & Separator & "templates"
			);
	-- Store all required templates.


private


	


	package Tag_Maps is new Ada.Containers.Ordered_Maps(
					Key_Type	=> Unbounded_String,
					Element_Type	=> Templates_Parser.Tag,
					"="		=> Templates_Parser."="
				);

	type Theme_Service is new Service_Type with record
		Mapping			: Unbounded_String;
		Component_Name		: Unbounded_String;
		Default_Theme_Name	: Unbounded_String;
		Template_Extension	: Unbounded_String;
	end record;
end KOW_View.Themes;
