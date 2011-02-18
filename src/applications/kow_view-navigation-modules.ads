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
-- Modules for navigation component                                         --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Locales;
with KOW_View.Components;
with KOW_View.Modules;
with KOW_View.Modules.Stateful_Module_Factories;
with KOW_View.Navigation.Components;

---------
-- AWS --
---------
with AWS.Status;

package KOW_View.Navigation.Modules is



	type Menu_Item_Type is record
		Label	: Unbounded_String;
		Href	: Unbounded_String;
	end record;

	package Menu_Item_Vectors is new Ada.Containers.Vectors(
					Index_Type	=> Positive,
					Element_Type	=> Menu_item_Type
				);

	type Menu_Module is new KOW_View.Modules.Module_Type with record
		Items		: Menu_Item_Vectors.Vector;
		Items_For	: KOW_Sec.User_Identity_Type := ( others => ' ' );
		-- used to update the items list whenever we have other user in the current session
		-- 	1. it could be thanks to a login procedure
		-- 	2. it could be thanks to a switch user procedure...

		Locale		: KOW_Lib.Locales.Locale;
		-- used by initialize request everytime it's called so
		-- we will check if there is a need to update the item

		Is_Initialized	: Boolean := False;


		Config		: KOW_Config.Config_File;
		-- store the page config internally
		-- this is to avoid infinite looping
	end record;

	overriding
	procedure Initialize_Request(
				Module		: in out Menu_Module;
				Request		: in     AWS.Status.Data;
				Config		: in out KOW_Config.Config_File
			);
	
	overriding
	procedure Process_Body(
				Module		: in out Menu_Module;
				Request		: in     AWS.Status.Data;
				Response	:    out Unbounded_String
			);
	-- return a html list (ul) with the given menu
	

	procedure Initialize_Menu_Items(
				Module		: in out Menu_Module;
				Request		: in     AWS.Status.Data
			);
	-- initialize all the menu items.
	-- this can be overriden by your own implementation
	--
	-- is called during the Proces_Body request to avoid infite looping

	package Menu_Factories is new KOW_View.Modules.Stateful_Module_Factories(
					Module_Type	=> Menu_Module,
					Component	=> KOW_View.Navigation.Components.Component'Access
				);

end KOW_View.Navigation.Modules;
