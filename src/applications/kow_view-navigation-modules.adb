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
		-- build up the menu item vector for the selected locale
		-- when the locale changes, update the list
		use KOW_Lib.Locales;

		Current_Locale : KOW_Lib.Locales.Locale := KOW_View.Locales.Get_Locale( Request );
	begin
		if Module.Is_Initialized and then Module.Locale = Current_Locale then
			return;
		end if;

		Menu_Item_Vectors.Clear( Module.Items );
		Module.Is_Initialized := True;
		Module.Locale := Current_Locale;
		
		declare
			Items	: KOW_Config.Config_File_Array := KOW_Config.Elements_Array( Config, "item" );
			Page_Protocol : constant String := "page://";

			function Is_Page( Str : in String ) return Boolean is
			begin
				return Str'Length > Page_Protocol'Length and then Str( Str'First .. Str'First + Page_Protocol'Length - 1 ) = Page_Protocol;
			end Is_Page;

			function Has_Access( Str : in String ) return Boolean is
				Page : constant String := Str( Str'First + Page_protocol'Length .. Str'Last );
				
				use KOW_View.Pages.Services;
				Service : Page_Service;
				-- luckly I don't need to call the page service from a instance created by any of the elements in here..
				-- why? for several reasons...
				-- 	1. the new process_request declared in kow_view.pages don't do anything but initializing
				-- 	2. we are not actually processing the page.. we just want the access rules checked...
				Dumb_Request  : AWS.Status.Data;
				Dumb_Response : AWS.Response.Data;
			begin
				Process_Custom_Request(
						Service		=> Service,
						Request		=> Dumb_Request,
						Response	=> Dumb_Response,
						Page		=> Page,
						Initialize_Only	=> True
					);
				return true;
			exception
				when KOW_Sec.Access_Denied => 
					return false;
			end Has_Access;
		begin
			for i in Items'Range loop
				declare
					Href	: constant String := KOW_Config.Element( Items( i ), "href" );
					Menu_Item : Menu_Item_Type;
				begin
					if Is_Page( Href ) and then Has_Access( Href ) then
						Menu_Item.Label := KOW_Config.Element(
										F		=> Items( i ),
										Key		=> To_Unbounded_String( "label" ),
										L_Code		=> Module.Locale.Code,
										Dump_On_Error	=> True
									);
						Menu_Item.Href  := To_Unbounded_String( "/pages/page/" );
						Append( Menu_Item.Href, Href( Href'First + Page_Protocol'Length .. Href'Last ) );

						Menu_Item_Vectors.Append( Module.Items, Menu_Item );
					else
						Menu_Item.Label := KOW_Config.Element(
										F		=> Items( i ),
										Key		=> To_Unbounded_String( "label" ),
										L_Code		=> Module.Locale.Code,
										Dump_On_Error	=> True
									);
						Menu_Item.Href := To_Unbounded_String( Href );
						Menu_Item_Vectors.Append( Module.Items, Menu_Item );
					end if;
				end;
			end loop;
		end;

	end Initialize_Request;



	overriding
	procedure Process_Body(
				Module		: in out Menu_Module;
				Request		: in     AWS.Status.Data;
				Response	:    out Unbounded_String
			) is
		-- return a html list (ul) with the given menu
		Buffer : Unbounded_String := To_Unbounded_String( "<ul>" );

		procedure Iterator( C : in Menu_Item_Vectors.Cursor ) is
			Menu_Item : Menu_Item_Type := Menu_Item_Vectors.Element( C );
		begin
			Append( Buffer, "<li>" );
			Append( Buffer, "<a href=""" );
				Append( Buffer, KOW_Lib.String_Util.Scriptify( To_String( Menu_Item.Href ) ) );
			Append( Buffer, """>" );
			Append( Buffer, Menu_Item.Label );
			Append( Buffer, "</a></li>" );
		end Iterator;
	begin
		Menu_Item_Vectors.Iterate( Module.Items, Iterator'Access );
		Append( Buffer, "</ul>" );
		Response := Buffer;
	end Process_Body;

end KOW_View.Navigation.Modules;
