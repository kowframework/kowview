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
with KOW_Lib.Locales;
with KOW_View.Components;	use KOW_View.Components;
with KOW_View.Module_Factories;
with KOW_View.Pages;		use KOW_View.Pages;
with KOW_View.Modules;


generic
	with package Modules is new KOW_View.Modules(<>);
package KOW_View.Navigation_Modules is


	-------------------
	-- The Menu Item --
	-------------------


	subtype Menu_Name is KOW_View.Name_Type;
	-- the name of the menu item is used to get labels from configuration files

	subtype Menu_Href is KOW_View.Name_Type;
	-- the href can be any kind of valid HTML href attribute for the a tag.
	-- but when it's a local URI, starting with /, then the menu
	-- module should be able to retrieve the dispatcher and check if the user
	-- has access to given address.


	type Menu_Item_Type is record
		Name			: Menu_Name	:= No_Name;
		Href			: Menu_Href	:= No_Name;
		Is_Active		: Boolean	:= False;
		Child_Items		: KOW_lib.Json.Array_Type;
	end record;

	function To_Json( Item : in Menu_Item_Type ) return KOW_Lib.Json.Object_Type;
	-- conver the menu item, with child items, into a json object
	
	function To_Menu_Item( Object : in KOW_Lib.Json.Object_Type ) return Menu_Item_Type;
	-- conver the item from a json object to a menu item type


	procedure Append(
				Item	: in out Menu_Item_Type;
				Child	: in     Menu_Item_Type
			);
	-- append the child into the menu item



	---------------------
	-- The Menu Module --
	---------------------

	type Menu_Module is abstract new Modules.KTML_Module with record
		Items	: KOW_Lib.Json.Array_Type;
		-- the module itens are stored in json format
		-- initialized by setup_items method (that should be overriden)
		
	
		Labels	: KOW_Config.Config_File_Type;
		-- initialized by setup_labels method
		--
		-- the labels should be index by menu item names; it's a simple map
		-- 	name	=> label
		--
		-- using the KOW Config syntax, of course.
	end record;

	overriding
	procedure Process_Request(
				Module	: in out Menu_Module;
				Page	: in out KOW_View.Pages.Page_Interface'Class;
				Status	: in     Request_Status_Type
			);
	-- build the menu HTML

	overriding
	procedure Process_Json_Request(
				Module	: in out menu_Module;
				Page	: in out KOW_View.Pages.Page_interface'Class;
				Status	: in     Request_Status_Type;
				Response:    out KOW_Lib.Json.Object_Type
			);
	-- return the menu structure in json format


	procedure Append(
				Menu	: in out Menu_Module;
				Item	: in     Menu_Item_Type
			);
	-- append a menu item


	function To_Json(
				Menu	: in Menu_Module;
				Status	: in Request_Status_Type
			) return KOW_Lib.Json.Object_Type;
	-- conver the menu into a json object
	-- only accessible items will be listed
	-- setup the label property in each item using the current locale



	procedure Setup(
				Menu	: in out Menu_Module;
				Status	: in     Request_Status_Type
			);
	-- check if menu.items has elements; if not, call both setup_items and setup_labels

	procedure Setup_Items(
				Menu	: in out Menu_Module;
				Status	: in     Request_Status_Type
			) is abstract;
	-- initialize the menu items in the module
	-- called only when the itens are not initialized; usefull when working with
	-- factories for stateful modules

	procedure Setup_Labels(
				Menu	: in out Menu_Module;
				Status	: in     Request_Status_Type
			);
	-- initialize the menu.labels property



	procedure Is_Allowed_And_Active(
				Menu	: in     Menu_Module;
				Status	: in     Request_Status_Type;
				Item	: in out Menu_Item_Type;
				Allowed	:    out Boolean;
			) return Boolean;
	-- check if the menu item is accessible (local items always checked; remote items never checked)


	function Get_Label(
				Menu	: in     Menu_Module;
				Item	: in     Menu_Item_Type;
				Locale	: in     KOW_Lib.Locales.Locale_Type
			) return String;
	-- get the label to be used to build the interface
end KOW_View.Navigation_Modules;

