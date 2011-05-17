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


------------------------------------------------------------------------------
-- Modules for navigation component                                         --
------------------------------------------------------------------------------

--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Locales;
with KOW_Lib.String_Util;
with KOW_Sec;
with KOW_View.Components;
with KOW_View.Locales;
with KOW_View.Modules;
with KOW_View.Modules.Stateful_Module_Factories;
with KOW_View.Pages.Services;
with KOW_View.Security;
with KOW_View.Services.Util;
with KOW_View.URI_Util;

---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;

package body KOW_View.Navigation.Modules is

	overriding
	procedure Initialize_Request(
				Module		: in out Menu_Module;
				Request		: in     AWS.Status.Data;
				Config		: in out KOW_Config.Config_File
			) is
	begin
		Module.Config := Config;
		Module.Dijit_Menu_Bar := KOW_Config.Value( Config, "dijit_menu_bar", True );
	end Initialize_Request;



	overriding
	procedure Process_Body(
				Module		: in out Menu_Module;
				Request		: in     AWS.Status.Data;
				Response	:    out Unbounded_String
			) is
		-- return a html list (ul) with the given menu
		Current_Level	: Positive := 1;
		URI		: constant String := AWS.Status.URI( Request );


		procedure Append_Disabled( Menu_Item : in Menu_ITem_Type ) is
		begin
			if Menu_Item.Disable_When_Active and then Is_Active( Menu_Module'Class( Module ), Request, Menu_Item ) then
				Append( Response, " disabled" );
			end if;
		end Append_Disabled;

		procedure Dijit_Iterator( C : in Menu_Item_Vectors.Cursor ) is
			Menu_Item : Menu_Item_Type := Menu_Item_Vectors.Element( C );
		begin
			if Current_Level < Menu_Item.Level then
				raise CONSTRAINT_ERROR with "Menu structure is all wrong... you should probably fix it";
			end if;
			-- we can't find a sub menu before a popupmenubaritem!

			while Current_Level > Menu_Item.Level loop
				-- close the previous sub menus..
				Append( Response, "</div></div>" );
				Current_level := Current_Level - 1;
			end loop;

			if Menu_Item.Href = "" then
				if Current_Level = 1 then
					Append( Response, "<div dojoType=""dijit.PopupMenuBarItem"">" );
				else
					Append( Response, "<div dojoType=""dijit.PopupMenuItem"">" );
				end if;
				Append( Response, "<span>" );
				Append( Response, Menu_Item.Label );
				Append( Response, "</span>" );

				Append( Response, "<div dojoType=""dijit.Menu"">" );
				Current_Level := Current_Level + 1;
			else
				if Current_Level = 1 then
					Append( Response, "<div dojoType=""dijit.MenuBarItem"" " );
				else
					Append( Response, "<div dojoType=""dijit.MenuItem"" " );
				end if;
				
				Append( Response, "onClick=""document.location.href='" );
				Append( Response, Menu_Item.Href );
				Append( Response, "'""" );

				Append_Disabled( Menu_Item );

				Append( Response, ">" );
				Append( Response, Menu_Item.Label );
				Append( Response, "</div>" );
			end if;
		end Dijit_Iterator;


		procedure Iterator( C : in Menu_Item_Vectors.Cursor ) is
			Menu_Item : Menu_Item_Type := Menu_Item_Vectors.Element( C );

			function Level return String is
			begin
				return Ada.Strings.Fixed.Trim( Positive'Image( Menu_Item.Level ), Ada.Strings.Both );
			end Level;
		begin
			Append( Response, "<li class=""menu_" & Level & """>" );
			Append( Response, "<a href=""" );
				Append( Response, KOW_Lib.String_Util.JSon_Scriptify( To_String( Menu_Item.Href ) ) );
			Append( Response, """" );
			Append_Disabled( Menu_Item );
			Append( Response, ">" );
			Append( Response, Menu_Item.Label );
			Append( Response, "</a></li>" );
		end Iterator;


	begin
		Initialize_Menu_Items( Menu_Module'Class( Module ), Request );


		if Module.Dijit_Menu_Bar then
			Include_Dojo_Package( Module, "dijit.Menu" );
			Include_Dojo_Package( Module, "dijit.MenuBar" );
    			Include_Dojo_Package( Module, "dijit.MenuBarItem");
			Include_Dojo_Package( Module, "dijit.MenuItem" );
			Include_Dojo_Package( Module, "dijit.PopupMenuBarItem" );
			Include_Dojo_Package( Module, "dijit.PopupMenuItem" );



		
			Append( Response, "<div dojoType=""dijit.MenuBar"">" );
			Menu_Item_Vectors.Iterate( Module.Items, Dijit_Iterator'Access );
				while Current_Level > 1 loop
					-- close the ramining sub menus
					Append( Response, "</div></div>" );
					Current_level := Current_Level - 1;
				end loop;

			Append( Response, "</div>" );

		else
			Append( Response, "<ul class=""menu"">" );
			Menu_Item_Vectors.Iterate( Module.Items, Iterator'Access );
			Append( Response, "</ul>" );
		end if;
	end Process_Body;



	procedure Initialize_Menu_Items(
				Module		: in out Menu_Module;
				Request		: in     AWS.Status.Data
			) is
		-- initialize all the menu items.
		-- this can be overriden by your own implementation
		--
		-- is called during the Proces_Body request to avoid infite looping
		use KOW_Lib.Locales;

		Current_Locale : KOW_Lib.Locales.Locale := KOW_View.Locales.Get_Locale( Request );

	begin
		if Module.Is_Initialized and then Module.Locale = Current_Locale then
			declare
				use KOW_Sec;
				User : KOW_Sec.User_Type := KOW_View.Security.Get_User( Request );
			begin
				if Module.Items_For = User.Data.Identity then
					return;
				else
					Module.Items_For := User.Data.Identity;
				end if;
			end;
		end if;

		Menu_Item_Vectors.Clear( Module.Items );
		Module.Is_Initialized := True;
		Module.Locale := Current_Locale;
		
		declare
			Items		: KOW_Config.Config_File_Array := KOW_Config.Elements_Array( Module.Config, "item" );
		begin
			for i in Items'Range loop
				declare
					Menu_Item : Menu_Item_Type := New_Menu_Item(
										Module		=> Menu_Module'Class( Module ),
										Request		=> Request,
										Item_ID		=> i,
										Menu_Config	=> Items( i )
									);
				begin
					if Menu_Item.Has_Access then
						Menu_Item_Vectors.Append( Module.Items, Menu_Item );
					end if;
				end;
			end loop;
		end;
	end Initialize_Menu_Items;

	function New_Menu_Item(
				Module		: in     Menu_Module;
				Request		: in     AWS.Status.Data;
				Item_ID		: in     Positive;
				Menu_Config	: in     KOW_Config.Config_File
			) return Menu_Item_Type is
		use KOW_View.URI_Util;

		Current_Page	: constant String := To_String( Module.Context );
		Href		: constant String := KOW_Config.Value( Menu_Config, "href", "" );
		Menu_Item	: Menu_Item_Type;

		function Has_Access( Str : in String ) return Boolean is
			
			Page : constant String := KOW_View.URI_Util.Get_Page_Name( Str );
			
			use KOW_View.Pages.Services;
			Service : Page_Service;
			-- luckly I don't need to call the page service from a instance created by any of the elements in here..
			-- why? for several reasons...
			-- 	1. the new process_request declared in kow_view.pages don't do anything but initializing
			-- 	2. we are not actually processing the page.. we just want the access rules checked...
			Dumb_Response : AWS.Response.Data;

		begin
			if Page = Current_Page then
				-- it's fine to assume this module is going to be initialized
				-- only after the page security has been aproved
				return true;
			end if;

			Process_Custom_Request(
					Service		=> Service,
					Request		=> Request,
					Response	=> Dumb_Response,
					Page		=> Page,
					Initialize_Only	=> True
				);
			return true;
		exception
			when KOW_Sec.Access_Denied | KOW_Sec.Login_Required => 
				return false;
		end Has_Access;
	begin
		Menu_Item.ID := Item_ID;
		Menu_Item.Disable_When_Active := KOW_Config.Value( Menu_Config, "disable_when_active", True );
		Menu_Item.Label := KOW_Config.Element(
						F		=> Menu_Config,
						Key		=> To_Unbounded_String( "label" ),
						L_Code		=> Module.Locale.Code,
						Dump_On_Error	=> True
					);
		Menu_Item.Level := KOW_Config.Value( Menu_Config, "level", 1 );

		if Is_Page_URN( Href ) then
			Menu_Item.Has_Access := Has_Access( Href );
			Menu_Item.Href  := To_Unbounded_String( To_Page_URI( Href ) );
		else
			Menu_Item.Has_Access := True;
			Menu_Item.Href := To_Unbounded_String( Href );
		end if;

		return Menu_Item;
	end New_Menu_Item;



	function Is_Active(
				Module		: in     Menu_Module;
				Request		: in     AWS.Status.Data;
				Menu_Item	: in     Menu_Item_Type
			) return Boolean is
		-- used only when disable_when_active is set in the item
		URI		: constant String := AWS.Status.URI( Request );
	begin
		return  Menu_Item.Href = URI or else Menu_Item.Href = URI & "/main";
	end Is_Active;


	--------------------------
	-- Module Switcher Menu --
	--------------------------
	
	overriding
	procedure Initialize_Request(
				Module		: in out Module_Switcher_Menu_Module;
				Request		: in     AWS.Status.Data;
				Config		: in out KOW_Config.Config_File
			) is
	begin
		Module.Preserve_Variables := KOW_Lib.String_Util.Explode( ',', KOW_Config.Value( Config, "preserve_variables", Null_Unbounded_String ) );


		Module.Default_Item	:= Positive'Value( KOW_Config.Value( Config, "default_item", "1" ) );
		-- the default item to be accepted as selected

		Module.Selector_Variable:= KOW_Config.Value( Config, "selector_variable", To_Unbounded_String( "selected_module_id" ) );
		-- the variable where should be stored the current selected module 


		Initialize_Request(
				Module	=> Menu_Module( Module ),
				Request	=> Request,
				Config	=> Config
			);
	end Initialize_Request;


	overriding
	function New_Menu_Item(
				Module		: in     Module_Switcher_Menu_Module;
				Request		: in     AWS.Status.Data;
				Item_ID		: in     Positive;
				Menu_Config	: in     KOW_Config.Config_File
			) return Menu_Item_Type is
		-- initialize each menu item...
		Menu_Item	: Menu_Item_Type;

		P		: AWS.Parameters.List := AWS.Status.Parameters( Request );

		procedure Append_Preserved_Variables( C : in KOW_Lib.UString_Vectors.Cursor ) is
			K : constant String := To_String( KOW_Lib.UString_Vectors.Element( C ) );
			V : constant String := AWS.Parameters.Get( P, K );
		begin
			Append( Menu_Item.Href, '&' & K & '=' & V );
		end Append_Preserved_Variables;
	begin
		Menu_item.ID := Item_ID;

		Menu_Item.Disable_When_Active := KOW_Config.Value( Menu_Config, "disable_when_active", True );
		Menu_Item.Label := KOW_Config.Element(
						F		=> Menu_Config,
						Key		=> To_Unbounded_String( "label" ),
						L_Code		=> Module.Locale.Code,
						Dump_On_Error	=> True
					);
		Menu_Item.Level := KOW_Config.Value( Menu_Config, "level", 1 );
		Menu_Item.Has_Access := True;	-- TODO :: check if the user can access the module in the future


		Append( Menu_Item.Href, "?" );
		Append( Menu_Item.Href, Module.Selector_Variable );
		Append( Menu_item.Href, "=" & Ada.Strings.Fixed.Trim( Positive'Image( Menu_Item.ID ), Ada.Strings.Both ) );

		KOW_Lib.UString_Vectors.Iterate( Module.Preserve_Variables, Append_Preserved_Variables'Access );

		return Menu_Item;
	end New_Menu_Item;

	
	overriding
	function Is_Active(
				Module		: in     Module_Switcher_Menu_Module;
				Request		: in     AWS.Status.Data;
				Menu_Item	: in     Menu_Item_Type
			) return Boolean is
	begin
		return Selected_Module( Module, AWS.Status.Parameters( Request ) ) = Menu_Item.ID;
	end Is_Active;


	function Selected_Module(
				Module		: in     Module_Switcher_Menu_Module;
				Parameters	: in     AWS.Parameters.List
			) return Positive is
		Parm : constant String := AWS.Parameters.Get( Parameters, To_String( Module.Selector_Variable ) );
	begin
		if Parm = "" then
			return Module.Default_Item;
		else
			return Positive'Value( Parm );
		end if;
	exception
		when CONSTRAINT_ERROR =>
			return Module.Default_Item;
	end Selected_Module;




end KOW_View.Navigation.Modules;
