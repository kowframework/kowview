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
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_View.Locales;
with KOW_View.Util;



package body KOW_View.Components is
	---------------------
	-- Components Name --
	---------------------

	function To_Name( Str : in String ) return Component_Name is
		-- convert from string to component name
		Name : Component_Name;
	begin
		KOW_Lib.String_Util.Copy( From => Str, To => String( Name ) );
		return Name;
	end To_Name;

	function To_String( Name : in Component_Name ) return String is
		-- trim the name and return it as a simple string
	begin
		return Ada.Strings.Fixed.Trim( String( Name ), Ada.Strings.Right );
	end To_String;


	---------------
	-- Component --
	---------------

	function Locate_Resource(
			Component	: in Component_Type;
			Status		: in Request_Status_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
		-- locate a resource file for this component
		-- this file should be placed at
		-- 	[WORKING_DIR]/data/component_name/resource.extension
		-- 	or
		-- 	[WORKING_DIR]/applications/component_name/data/resource.extension
		-- returning it's name if nothing has been found raise Ada.Directories.Name_Error if not found



		use Ada.Directories;
		use KOW_Lib.File_System;
		use KOW_Lib.Locales;

		CName		: constant String	:= To_String( Component.Name );
		Name		: constant String	:= "data" / CName / Resource; --& "." & Extension;
		Default_Name	: constant String	:= "components" / CName / Resource;-- & "." & Extension;

		Virtual_Host	: constant String	:= KOW_View.Virtual_Host( Status );
		Locale		: constant Locale_Type	:= KOW_View.Locales.Get_Locale( Status );

		function Virtual_Host_Name return String is
			pragma Inline( Virtual_Host_Name );

			-- declared inside a function so it's never be computed if
			-- virtual host is disabled
		begin
			return "vhost"/Virtual_Host/CName/Resource;
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


			function inner_check( LC : in Locale_Code_Type ) return String is
				N : constant String := FName & '_' & To_String( LC ) & '.' & Extension;
			begin
				if Check( N ) then
					return N;
				else
					return "";
				end if;
			end inner_check;

			The_Name : constant String := Inner_Check( Locale.Code );
		begin
			if The_Name /= "" then
				return The_Name;
			else
				return Inner_Check( ( Language => Locale.Code.Language, Country => No_Country ) );
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

			declare
				use kOW_View.Components;
				Name : constant String := To_String( Component.Name );
			begin
				raise Ada.Directories.Name_Error with "Resource " & Resource & "." & Extension & " of component " & Name & " not found!";
			end;
		end LMC;

	begin
		-- Notice the parameter in LMC function actually calls the "+" function which is responsible for checking
		-- if the string is found
		if KOW_View.Enable_Virtual_Host and then Virtual_Host /= "" then
			return LMC( "" + Virtual_Host_Name + Name + Default_Name );
			-- check the virtual host name, then name then default name
		else
			return LMC( "" + Name + Default_Name );
			-- check the name then default name
		end if;

	end Locate_Resource;



	function Load_Config(
			Component	: in Component_Type;
			N		: in String := "setup"
		) return KOW_Config.Config_File_Type is
		-- load the configuration file
		use KOW_Lib.File_System;
	begin
		return KOW_Config.New_Config_File( To_String( Component.Name ) / N, False );
	end Load_Config;



	function New_Component(
				Name	: in String
			) return Component_Ptr is
		-- allocate and initialize the component
		-- use this to declare your own components
		Component : Component_Access := new Component_Type'( Name => To_Name( Name ) );
	begin
		return Component_Ptr( Component );
	end New_Component;

end KOW_View.Components;
