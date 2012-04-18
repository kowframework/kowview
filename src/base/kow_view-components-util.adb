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

--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;
with KOW_Lib.Locales;
with KOW_Lib.String_Util;
with KOW_View.Util;


package body KOW_View.Components.Util is


	function Get_Name( Component_Tag : in Ada.Tags.Tag ) return String is
	begin
		return To_String( KOW_View.Util.Get_Type_Name(
						Component_Tag,
						"_component"
					) );
	end Get_Name;


	
	function Get_Name( URI : in String ) return String is
                function Last_URI_Boundary return Integer is
                begin

                        for i in URI'First + 1 .. URI'Last loop
                                if URI( i ) = '/' then
                                        return i - 1;
                                end if;
                        end loop;


                        -- if got here, the URI is the service mapping:
                        return URI'Last;

                end Last_URI_Boundary;

		First : Positive := URI'First;

        begin
		if URI( First ) = '/' then
			First := First + 1;
		end if;

		return URI( First .. Last_URI_Boundary );
	end Get_Name;


	function Locate_Resource(
			Component_Name	: in String;
			Resource	: in String;
			Extension	: in String;
			Virtual_Host	: in String;
			Kind		: in Ada.Directories.File_Kind;
			Locale_Code	: in KOW_Lib.Locales.Locale_Code_Type := KOW_Lib.Locales.Get_Default_Locale_Code
		) return String is
		-- locate a resource file for this component
		-- this file should be placed at
		-- 	[WORKING_DIR]/data/kowview/component_name/resource.extension
		-- 	or
		-- 	[WORKING_DIR]/applications/component_name/data/resource.extension
		-- returning it's name if nothing has been found raise Ada.Directories.Name_Error if not found



		use Ada.Directories;
		use KOW_Lib.File_System;

		MComponent_Name	: String		:= KOW_Lib.String_Util.Str_Replace( From => '.', To => '-', Str => Component_Name );

		Name		: String		:= "data" / "kowview" / MComponent_Name / Resource; --& "." & Extension;
		Default_Name	: String		:= "applications" / MComponent_Name / "data" / Resource;-- & "." & Extension;

		function Virtual_Host_Name return String is
		begin
			return "data"/"kowview"/"vhost"/Virtual_Host/MComponent_Name/Resource;
		end Virtual_Host_Name;





		function check( FName : in String ) return Boolean is
		begin
			
			if not Ada.Directories.Exists( FName ) then
				return false;
			elsif Ada.Directories.Kind( FName ) /= Kind then
				return false;
			else
				return True;
			end if;
		end Check;

		function Check_Localized( FName : in String ) return String is
			use Ada.Strings;
			use Ada.Strings.Unbounded;
			use KOW_Lib.Locales;


			function inner_check( LC : in Locale_Code_Type ) return String is
				N : constant String := FName & '_' & To_String( LC ) & '.' & Extension;
			begin
				if Check( N ) then
					return N;
				else
					return "";
				end if;
			end inner_check;

			The_Name : constant String := Inner_Check( Locale_Code );
		begin
			if The_Name /= "" then
				return The_Name;
			else
				return Inner_Check( ( Language => Locale_Code.Language, Country => No_Country ) );
			end if;
		end Check_Localized;
	


		function "+"( L, R : in String ) return String is
			Computed : String renames L;
			The_name : String renames R;
		begin
			if Computed = "" then
				return Check_Localized( The_Name );
			else
				return Computed;
			end if;
		end "+";

		function LMC( Str : in String ) return String is
			-- it's a last minute check; raise exception if the resource is not found
		begin
			if Str /= "" then
				return Str;
			end if;

			raise Ada.Directories.Name_Error with "Resource "+Resource+ "." + Extension + " of component " + Component_name + " not found!";
		end LMC;

	begin

		if KOW_View.Enable_Virtual_Host then
			return LMC( "" + Virtual_Host_Name + Name + Default_Name );
		else
			return LMC( "" + Name + Default_Name );
		end if;

	end Locate_Resource;



	function Load_Main_Configuration(
			Component_Name	: in String
		) return KOW_Config.Config_File_Type is
		-- load the main configuration for this component
		use KOW_Lib.File_System;
	begin
		return KOW_Config.New_Config_File(
				N => "kowview" / Component_Name
			);
	end Load_Main_Configuration;



	function Load_Configuration(
			Component_Name		: in String;
			Configuration_Name	: in String
		) return KOW_Config.Config_File_Type is
		-- load a configuration file from this component's relative path
		use KOW_Lib.File_System;
	begin
		return KOW_Config.New_Config_File(
				N => "kowview" / Component_Name / Configuration_Name
			);
	end Load_Configuration;



end KOW_View.Components.Util;
