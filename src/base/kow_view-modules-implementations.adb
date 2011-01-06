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
pragma License (GPL);


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_View.Modules;

---------
-- AWS --
---------
with AWS.Status;

package body KOW_View.Modules.Implementations is

	overriding
	procedure Initialize_Request(
			Module		: in out Resource_Module;
			Request		: in     AWS.Status.Data;
			Config		: in out KOW_Config.Config_File
		) is
	begin
		Module.Resource := KOW_Config.Element( Config, "resource" );
	end Initialize_Request;


	overriding
	procedure Process_Body(
				Module	: in out Resource_Module;
				Request	: in     AWS.Status.Data;
				Response:    out Unbounded_String
			) is
		-- return the content specified by the configuration
		Buffer : Unbounded_String;

		Resource_URI	: constant String := To_String( Module.Resource );
		Locale		: constant KOW_Lib.Locales.Locale := KOW_View.Locales.Get_Locale( Request );

		Resource	: constant String := Ada.Directories.Containing_Directory( Resource_URI ) & '/' &
							Ada.Directories.Base_Name( Resource_URI );
		Extension	: constant String := Ada.Directories.Extension( Resource_URI );

		Resource_Path	: constant String := Locate_Resource(
								Service		=> Service,
								Resource	=> Resource,
								Extension	=> Extension,
								Locale		=> Locale
							);
		File : Ada.Text_IO.File_Type;
	begin

		-- TODO :: use streams as it's a lot faster than text_io
		Ada.Text_IO.Open( File, Ada.Text_IO.In_File, Resource_Path );
		loop
			Ada.Text_IO.Get( File, Char );
			Append( Response, Char );
		end loop;
	exception
		when End_Error =>
			Ada.Text_IO.Close( File );
			Response := Buffer;

	end Process_Body;
end KOW_View.Modules.Implementations;
