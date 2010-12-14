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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_View.Util;


package body KOW_View.Components.Util is


	function Get_Name( Component_Tag : in Ada.Tags.Tag ) return String is
	begin
		return KOW_View.Util.Get_Type_Name(
						Component_Tag,
						"_component"
					);
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

        begin
		return URI( URI'First .. Last_URI_Boundary );
	end Get_Name;


	function Locate_Resource(
			Component_Name	: in String;
			Resource	: in String;
			Extension	: in String;
			Kind		: in Ada.Directories.File_Kind	
		) return String is
		-- locate a resource file for this component
		-- this file should be placed at
		-- 	[WORKING_DIR]/data/kowview/component_name/resource.extension
		-- 	or
		-- 	[WORKING_DIR]/applications/component_name/data/resource.extension
		-- returning it's name if nothing has been found raise Ada.Directories.Name_Error if not found
		-- TODO: Implement locale support at Locate_Resource function



		use Ada.Directories;
		use KOW_Lib.File_System;

		MComponent_Name	: String		:= KOW_Lib.String_Util.Str_Replace( From => '.', To => '-', Str => Component_Name );

		Name		: String		:= "data" / "kowview" / MComponent_Name / Resource & "." & Extension;
		Default_Name	: String		:= "applications" / MComponent_Name / "data" / Resource & "." & Extension;






		procedure check( FName : in String ) is
			Real_Kind : File_Kind;
		begin
			Real_Kind := Ada.Directories.Kind( FName );

			if Real_Kind /= Kind then
				raise Ada.Directories.Name_Error with
						"Resource """ & Resource &
						""" of component """ & Component_Name &
						""" is of type """ & File_Kind'Image( Real_Kind ) &
						""" ( expected """ & File_Kind'Image( Kind ) & """)";
			end if;
		exception
			when Ada.IO_Exceptions.Name_Error =>
				raise Ada.Directories.Name_Error with
					"Resource """ & Resource &
					""" of component """ & Component_Name &
					""" ( aka """ & FName & """ ) not found";
		end Check;


	begin

		Check( Name );

		return Name;
	exception
		when others =>
			Check( Default_Name );
			return Default_Name;

	end Locate_Resource;



	function Load_Main_Configuration(
			Component_Name	: in String
		) return KOW_Config.Config_File is
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
		) return KOW_Config.Config_File is
		-- load a configuration file from this component's relative path
		use KOW_Lib.File_System;
	begin
		return KOW_Config.New_Config_File(
				N => "kowview" / Component_Name / Configuration_Name
			);
	end Load_Configuration;



end KOW_View.Components.Util;
