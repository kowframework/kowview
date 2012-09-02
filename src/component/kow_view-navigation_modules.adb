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
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Locales;
with KOW_View.Module_Factories;
with KOW_View.Modules;
with KOW_View.Pages;			use KOW_View.Pages;
with KOW_View.Request_Dispatchers;


---------
-- AWS --
---------
with AWS.Status;


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
		Set( Object, "is_active", Item.Is_Active );
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
				Is_Active		=> Get( Object, "is_active" ),
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
		Response := To_Json( Module, Status );
	end Process_Json_Request;

	procedure Append(
				Menu	: in out Menu_Module;
				Item	: in     Menu_Item_Type
			) is
		-- append a menu item
	begin
		KOW_Lib.Json.Append( Menu.Items, To_Json( Item ) );
	end Append;

	function To_Json(
				Menu	: in Menu_Module;
				Status	: in Request_Status_Type
			) return KOW_Lib.Json.Object_Type is
		-- conver the menu into a json object
		use KOW_Lib.Json;
		Object	: Object_Type;
		Items	: Array_Type;

		Locale	: KOW_Lib.Locales.Locale_Type := KOW_View.Locales.Get_Locale( Status );


		procedure Append_Items(
					Into	: in out Array_Type;
					From	: in     Array_Type
				) is

			procedure Iterator(
						Index	: Natural;
						Data	: Json_Data_Type
					) is
				Item_Object	: Object_Type := From_Data( Data );
				Item		: Menu_Item_Type := To_Menu_Item( Item_Object );
				Child_Items	: Array_Type;
				Allowed		: Boolean;
			begin
				Is_Allowed_And_Active( Menu_Module'Class( Menu ), Status, Item, Allowed );
				if Allowed then
					Append_Items(
							Into	=> Child_Items,
							From	=> Item.Child_Items
						);
					Item.Child_Items := Child_Items;
	
					Item_Object := To_Json( Item );
					Set( Item_Object, "label", Get_Label( Menu_Module'Class( Menu ), Item, Locale ) );
	
					Append( Into, Item_Object );
				end if;
			end Iterator;
		begin

			Iterate( From, Iterator'Access );
		end Append_Items;
	begin
		Append_Items(
				Into	=> Items,
				From	=> Menu.Items
			);
		KOW_Lib.Json.Set( Object, "items", Items );
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



	procedure Is_Allowed_And_Active(
				Menu	: in     Menu_Module;
				Status	: in     Request_Status_Type;
				Item	: in out Menu_Item_Type;
				Allowed	:    out Boolean
			) is
		-- check if the menu item is accessible (local items always checked; remote items never checked)
		use KOW_View.Request_Dispatchers;
		Dispatcher : Request_Dispatcher_Ptr;
		URL	: constant String := To_String( Item.Href );
	begin
		if Item.Href(1) = '/' then
			Dispatcher := Get_Dispatcher( URL );
			if Dispatcher = null then
				raise BROKEN_LINK with To_String( Item.Name ) & " point to a invalid local URI: '" & To_String( Item.Href ) & ''';
			end if;

			Allowed := Is_Allowed( Dispatcher.all, Status.Request );
			Item.Is_Active := URL = AWS.Status.URI( Status.Request );
		else
			Allowed := True;
		end if;
	end Is_Allowed_And_Active;

	function Get_Label(
				Menu	: in     Menu_Module;
				Item	: in     Menu_Item_Type;
				Locale	: in     KOW_Lib.Locales.Locale_Type
			) return String is
		-- get the label to be used to build the interface
		use KOW_Config;
		Name : constant String := To_String( Item.Name );
	begin
		if Contains( Menu.Labels, Name ) then
			return Value( Menu.Labels, Name, Locale.Code );
		else
			return "[" & name & "]";
		end if;
	end Get_Label;

end KOW_View.Navigation_Modules;

