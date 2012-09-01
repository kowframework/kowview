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

--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;



-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_View.Components;	use KOW_View.Components;
with KOW_View.Module_Factories;
with KOW_View.Pages;		use KOW_View.Pages;
with KOW_View.Modules;


package body KOW_View.Navigation_Modules is


	-------------------
	-- The Menu Item --
	-------------------



	function To_Json( Item : in Menu_Item_Type ) return KOW_Lib.Json.Object_Type is
		-- conver the menu item, with child items, into a json object
		use KOW_Lib.Json;
		Object : Object_Type;
	begin
		Set( Object, "name", To_String( Item.Name ) );
		Set( Object, "href", To_String( Item.Href ) );
		Set( Object, "disable_when_active", Item.Disable_When_Active );
		Set( Object, "has_access", Item.Has_Access );
		Set( Object, "child_items", Item.Child_Items );

		return Object;
	end To_Json;


	function To_Menu_Item( Object : in KOW_Lib.Json.Object_Type ) return Menu_Item_Type is
		-- conver the item from a json object to a menu item type
		use KOW_Lib.Json;
	begin
		return (
				Name			=> To_Name( Get( Object, "name" ) ),
				Href			=> To_Name( Get( Object, "href" ) ),
				Disable_When_Active	=> Get( Object, "disable_when_active" ),
				Has_Access		=> Get( Object, "has_access" ),
				Child_Items		=> Get( Object, "child_items" )
			);
	end To_Menu_Item;

	procedure Append(
				Item	: in out Menu_Item_Type;
				Child	: in     Menu_Item_Type
			) is
		-- append the child into the menu item
	begin
		KOW_Lib.Json.Append( Item.Child_items, To_Json( Child ) );
	end Append;


	---------------------
	-- The Menu Module --
	---------------------


	overriding
	procedure Process_Request(
				Module	: in out Menu_Module;
				Page	: in out KOW_View.Pages.Page_Interface'Class;
				Status	: in     Request_Status_Type
			) is
		-- build the menu HTML
	begin
		null;
		-- TODO :: implementme!
	end Process_Request;

	overriding
	procedure Process_Json_Request(
				Module	: in out menu_Module;
				Page	: in out KOW_View.Pages.Page_interface'Class;
				Status	: in     Request_Status_Type;
				Response:    out KOW_Lib.Json.Object_Type
			) is
		-- return the menu structure in json format
	begin
		Response := To_Json( Module );
	end Process_Json_Request;

	procedure Append(
				Menu	: in out Menu_Module;
				Item	: in     Menu_Item_Type
			) is
		-- append a menu item
	begin
		KOW_Lib.Json.Append( Menu.Items, To_Json( Item ) );
	end Append;

	function To_Json( Menu : in Menu_Module ) return KOW_Lib.Json.Object_Type is
		-- conver the menu into a json object
		Object : KOW_Lib.Json.Object_Type;
	begin
		KOW_Lib.Json.Set( Object, "items", Menu.Items );

		return Object;
	end To_Json;


	procedure Setup(
				Menu	: in out Menu_Module;
				Status	: in     Request_Status_Type
			) is
		-- check if menu.items has elements; if not, call both setup_items and setup_labels
	begin
		if KOW_Lib.Json.Count( Menu.Items ) = 0 then
			Setup_Items(
					Menu	=> Menu_Module'Class( Menu ),
					Status	=> Status
				);
			Setup_Labels(
					Menu	=> Menu_Module'Class( Menu ),
					Status	=> Status
				);
		end if;
	end Setup;


	procedure Setup_Labels(
				Menu	: in out Menu_Module;
				Status	: in     Request_Status_Type
			) is
		-- initialize the menu.labels property
	begin
		Menu.Labels := Load_Config(
						Module		=> Menu_Module'Class( Menu ),
						N		=> "labels"
					);
	end Setup_Labels;


end KOW_View.Navigation_Modules;

