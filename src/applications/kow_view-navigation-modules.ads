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
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;
with KOW_View.Modules;
with KOW_View.Modules.Stateful_Module_Factories;
with KOW_View.Modules.Stateless_Module_Factories;
with KOW_View.Navigation.Components;
with KOW_View.Pages.Services;

---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;

package KOW_View.Navigation.Modules is



	type Menu_Item_Type is record
		ID			: Positive;
		Label			: Unbounded_String;
		Level			: Positive := 1;
		Href			: Unbounded_String;
		Disable_When_Active	: Boolean := True;
		Has_Access		: Boolean := False;
	end record;

	package Menu_Item_Vectors is new Ada.Containers.Vectors(
					Index_Type	=> Positive,
					Element_Type	=> Menu_item_Type
				);
	
	-----------------
	-- Menu Module --
	-----------------

	type Menu_Module is new KOW_View.Modules.Module_Type with record
		Items		: Menu_Item_Vectors.Vector;
		Items_For	: KOW_Sec.User_Identity_Type := ( others => ' ' );
		-- used to update the items list whenever we have other user in the current session
		-- 	1. it could be thanks to a login procedure
		-- 	2. it could be thanks to a switch user procedure...

		Locale		: KOW_Lib.Locales.Locale_Type;
		-- used by initialize request everytime it's called so
		-- we will check if there is a need to update the item

		Is_Initialized	: Boolean := False;


		Config		: KOW_Config.Config_File_Type;
		-- store the page config internally
		-- this is to avoid infinite looping

		Dijit_Menu_Bar	: Boolean := True;

	end record;

	overriding
	procedure Initialize_Request(
				Module		: in out Menu_Module;
				Request		: in     AWS.Status.Data;
				Config		: in out KOW_Config.Config_File_Type
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


	function New_Menu_Item(
				Module		: in     Menu_Module;
				Request		: in     AWS.Status.Data;
				Item_ID		: in     Positive;
				Menu_Config	: in     KOW_Config.Config_File_Type
			) return Menu_Item_Type;
	-- initialize a given menu item

	function Is_Active(
				Module		: in     Menu_Module;
				Request		: in     AWS.Status.Data;
				Menu_Item	: in     Menu_Item_Type
			) return Boolean;
	-- used only when disable_when_active is set in the item

	package Menu_Factories is new KOW_View.Modules.Stateful_Module_Factories(
					Module_Type	=> Menu_Module,
					Component	=> KOW_View.Navigation.Components.Component'Access
				);


	--------------------------
	-- Module Switcher Menu --
	--------------------------
	
	type Module_Switcher_Menu_Module is new Menu_Module with record
		-- renders the menu for a module switcher container module

		Preserve_Variables	: KOW_Lib.UString_Vectors.Vector;
		-- which variable should be passed allong with the item links

		Default_Item		: Positive;
		-- the default item to be accepted as selected

		Selector_Variable	: Unbounded_String;
		-- the variable where should be stored the current selected module 
	end record;


	overriding
	procedure Initialize_Request(
				Module		: in out Module_Switcher_Menu_Module;
				Request		: in     AWS.Status.Data;
				Config		: in out KOW_Config.Config_File_Type
			);
	
	overriding
	function New_Menu_Item(
				Module		: in     Module_Switcher_Menu_Module;
				Request		: in     AWS.Status.Data;
				Item_ID		: in     Positive;
				Menu_Config	: in     KOW_Config.Config_File_Type
			) return Menu_Item_Type;
	-- initialize each menu item...
	
	
	overriding
	function Is_Active(
				Module		: in     Module_Switcher_Menu_Module;
				Request		: in     AWS.Status.Data;
				Menu_Item	: in     Menu_Item_Type
			) return Boolean;
	-- determine if the current section is the one being viewed
	

	function Selected_Module(
				Module		: in     Module_Switcher_Menu_Module;
				Parameters	: in     AWS.Parameters.List
			) return Positive;
	


	package Module_Switcher_Menu_Factories is new KOW_View.Modules.Stateless_Module_Factories(
					Module_Type	=> Module_Switcher_Menu_Module,
					Component	=> KOW_View.Navigation.Components.Component'Access
				);
	-- this one has got to be stateless because of the possibility of some variables changing in the page
				
	
	-------------------------------
	-- Module Switcher Container --
	-------------------------------

	type Module_Switcher_Container_Module is new KOW_View.Modules.Module_Type with record
		-- this module acts as a proxy for the actual module being called.
		--
		-- it uses the very same procedures and functions the page service uses for allocating and deallocating the child module
		--
		-- it's a new instance of module_type instead of module_interface so we can use the generic factory packages
		-- these packages initialize for us:
		-- 	. well, the module :)
		--	. ID
		--	. context
	
		Default_Item		: Positive;
		Selector_Variable	: Unbounded_String;
		Current			: KOW_View.Pages.Services.Complete_Module_Type;
	end record;


	overriding
	procedure Initialize_Request(
			Module		: in out Module_Switcher_Container_Module;
			Request		: in     AWS.Status.Data;
			Config		: in out KOW_Config.Config_File_Type
		);
	-- Initialize the processing of a request
	-- also loads the current module and such

	overriding
	function Get_Script_Includes(
			Module		: in     Module_Switcher_Container_Module
		) return KOW_Lib.UString_Vectors.Vector;

	overriding
	function Get_Dojo_Packages(
			Module		: in     Module_Switcher_Container_Module
		) return KOW_Lib.UString_Vectors.Vector;
	
	overriding
	function Get_Dojo_CSS(
			Module		: in     Module_Switcher_Container_Module
		) return KOW_Lib.UString_Vectors.Vector;

	overriding
	function Get_CSS_Includes(
			Module		: in     Module_Switcher_Container_Module
		) return KOW_Lib.UString_Vectors.Vector;


	overriding
	procedure Process_Head(
			Module		: in out Module_Switcher_Container_Module;
			Request		: in     AWS.Status.Data;
			Response	:    out Unbounded_String
		);
	-- process header of the response.
	-- it's assumed that 

	overriding
	procedure Process_Body(
			Module		: in out Module_Switcher_Container_Module;
			Request		: in     AWS.Status.Data;
			Response	:    out Unbounded_String
		);
	-- process the request for a module.
	-- sometimes is useful for a module only to be created and released - such as in a page counter module

	overriding
	procedure Process_Foot(
			Module		: in out Module_Switcher_Container_Module;
			Request		: in     AWS.Status.Data;
			Response	:    out Unbounded_String
		);
	-- process some footer of the module
	-- useful when creating benchmar modules


	overriding
	procedure Process_Json_Request(
			Module		: in out Module_Switcher_Container_Module;
			Request		: in     AWS.Status.Data;
			Response	: out    KOW_Lib.Json.Object_Type
		);

	overriding
	procedure Finalize_Request(
			Module		: in out Module_Switcher_Container_Module;
			Request		: in     AWS.Status.Data
		);
	-- Finalize processing the request.
	-- Called when the process has been finalized


	function Selected_Module(
				Module		: in     Module_Switcher_Container_Module;
				Parameters	: in     AWS.Parameters.List
			) return Positive;
	
	package Module_Switcher_Container_Factories is new KOW_View.Modules.Stateless_Module_Factories(
								Module_Type	=> Module_Switcher_Container_Module,
								Component	=> KOW_View.Navigation.Components.Component'Access
							);


end KOW_View.Navigation.Modules;
