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

				if Menu_Item.Disable_When_Active then
					if Menu_Item.Href = URI or else Menu_Item.Href = URI & "/main" then
						Append( Response, " disabled" );
					end if;
				end if;
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
				Append( Response, KOW_Lib.String_Util.Scriptify( To_String( Menu_Item.Href ) ) );
			Append( Response, """>" );
			Append( Response, Menu_Item.Label );
			Append( Response, "</a></li>" );
		end Iterator;


	begin
		Initialize_Menu_Items( Module, Request );


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
			Current_Page	: constant String := To_String( Module.Context );

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
			for i in Items'Range loop
				declare
					use KOW_View.URI_Util;
					Href	: constant String := KOW_Config.Value( Items( i ), "href", "" );
					Menu_Item : Menu_Item_Type;
				begin
			
					Menu_Item.Disable_When_Active := KOW_Config.Value( Items( i ), "disable_when_active", True );

					if Is_Page_URN( Href ) then
						if Has_Access( Href ) then
							Menu_Item.Label := KOW_Config.Element(
											F		=> Items( i ),
											Key		=> To_Unbounded_String( "label" ),
											L_Code		=> Module.Locale.Code,
											Dump_On_Error	=> True
										);
							Menu_Item.Level := KOW_Config.Value( Items( i ), "level", 1 );
							Menu_Item.Href  := To_Unbounded_String( To_Page_URI( Href ) );
	
							Menu_Item_Vectors.Append( Module.Items, Menu_Item );
						end if;
					else
						Menu_Item.Label := KOW_Config.Element(
										F		=> Items( i ),
										Key		=> To_Unbounded_String( "label" ),
										L_Code		=> Module.Locale.Code,
										Dump_On_Error	=> True
									);
						Menu_Item.Level := KOW_Config.Value( Items( i ), "level", 1 );
						Menu_Item.Href := To_Unbounded_String( Href );
						Menu_Item_Vectors.Append( Module.Items, Menu_Item );
					end if;
				end;
			end loop;
		end;


	end Initialize_Menu_Items;

end KOW_View.Navigation.Modules;
