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
with ada.text_io;


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;
with KOW_View.Components.Registry;
with KOW_View.Components.Util;
with KOW_View.Pages.Components;

package body KOW_View.Pages.Services.Util is





	function Get_Config_File( Page : in String ) return KOW_Config.Config_File is
		-- get the config file for the given page..
		Config : KOW_Config.Config_File;
	begin
		Page_Config_Cache.Get_Config_File( Config, Page );
		return Config;
	end Get_Config_File;


	function Get_Template( Config : in KOW_Config.Config_File ) return KOW_View.Themes.Template_Type is
		-- get the template for the given configuration file
		Tpl : Unbounded_String := KOW_Config.Element( Config, "template" );
	begin
		return KOW_View.Themes.Templates_Registry.Registry.Get( Tpl );
	end Get_Template;

	function Get_Modules( Config : in KOW_Config.Config_File ) return Complete_Module_Array is
		use KOW_Config;
		use KOW_View.Components;
		Elements	: Config_File_Array := Elements_Array( Config, "modules" );
		Modules		: Complete_Module_Array( Elements'Range );
	begin

		for i in Elements'Range loop
			ada.text_io.put_line( integer'image(i));
			Modules( i ) := Get_Module( Elements( i ) );
		end loop;

		return Modules;
	end Get_Modules;

	function Get_Module( Module_Config : in KOW_Config.Config_File ) return Complete_Module_Type is
		use KOW_Config;
		use KOW_View.Components;
		Component_Name	: Unbounded_String;
		Module		: Complete_Module_Type;
	begin
		Component_Name := Element( Module_Config, "component" );

		Ada.text_io.put_line( "allocating module for component " & to_string(component_name));
		Module.Config	:= Module_Config;
		Module.Factory := Module_Factory_Ptr( Get_Module_Factory(
							Component	=> Registry.Get_Component( Component_Name ).all,
							Name		=> Element( Module_Config, "module" )
						) );
		return Module;
	end Get_Module;


	function Get_Module_IDs(
				Config	: in KOW_Config.Config_File;
				Region	: in KOW_View.Themes.Region_Type
			) return Index_Array is
		use KOW_Lib.UString_Vectors;
		IDs_Str		: constant Unbounded_String := KOW_Config.Element( Config, Unbounded_String( Region ) );
		IDs_Vector	: constant Vector := KOW_Lib.String_Util.Explode( ',', IDs_Str );
		
		Results		: Index_Array( 1 .. Natural( Length( IDs_Vector ) ) );
		i		: Positive := 1;

		procedure iterator( C : in Cursor ) is
		begin
			Results( i ) := Positive'Value( To_String( Element( C ) ) );
			i := i + 1;
		end iterator;
	begin
		Iterate( IDs_Vector, Iterator'Access );
		return Results;
	end Get_Module_IDs;



-- private


	protected body Page_Config_Cache is
		procedure Get_Config_File( Config : out KOW_Config.Config_File; Page : in String ) is
		-- check if the config file is in the map... if not, read it into the map
		-- return the config file if available
			use KOW_Lib.File_System;
			
			UPage : constant Unbounded_String := To_Unbounded_String( Page );

			My_Config : KOW_Config.Config_File;

			procedure Load is
				-- load configuration from file in disk
			begin
				My_Config := KOW_View.Components.Util.Load_Configuration(
											Component_Name		=> KOW_View.Components.Get_Name( KOW_View.Pages.Components.Component ),
											Configuration_Name	=> "page" / Page
										);
				
				if KOW_Config.Has_Element( My_Config, "extends" ) then
					declare
						Parent : KOW_Config.Config_File;
					begin
						Get_Config_File( Parent, KOW_Config.Element( My_Config, "extends" ) );
						My_Config := KOW_Config.Merge_Configs( Parent => Parent, Child => My_Config );
					end;
				end if;
			end Load;

		begin
			if KOW_View.Pages.Components.Component.Enable_Cache then
				if Config_File_Maps.Contains( Cache, UPage ) then
					Config := Config_File_Maps.Element( Cache, UPage );
				else
					Load;
					Config := My_Config;
					Config_File_Maps.Include( Cache, UPage, My_Config );
				end if;
			else
				Load;
				Config := My_Config;
			end if;
		end Get_Config_File;
	end Page_Config_Cache;


end KOW_View.Pages.Services.Util;
