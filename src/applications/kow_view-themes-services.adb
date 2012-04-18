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

--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_View;
with KOW_View.Components;
with KOW_View.Components.Registry;
with KOW_View.Components.Util;
with KOW_View.Services;
with KOW_View.Services.Util;


---------
-- AWS --
---------
with AWS.MIME;
with AWS.Session;
with AWS.Status;
with AWS.Response;



package body KOW_View.Themes.Services is


	--------------
	-- Services --
	--------------

	overriding
	procedure Process_Custom_Request(
			Service		: in out Theme_Service;
			Status		: in     Request_Status_Type;
			Response	:    out AWS.Response.Data
		) is

		-- process request for a theme's static file
		-- only direct access to files that aren't template are alowed
	
		Theme_Name : constant String := Get_Theme_Name( Status.Request );

		URI		: constant string := KOW_View.Services.Util.Local_URI( Service, AWS.Status.URI( Status.Request ), True );
		Extension	: constant string := Ada.Directories.Extension( URI );
		File_Name	: constant string := URI( URI'First .. URI'Last - Extension'Length - 1);
		Complete_Path	: constant string := Locate_Theme_Resource(
				Theme_Name	=> Theme_Name,
				Resource	=> File_Name,
				Extension	=> Extension,
				Virtual_Host	=> KOW_View.Virtual_Host( Status.Request ),
				Kind		=> Ada.Directories.Ordinary_File
			);
	begin

		if Extension = To_String( Service.Template_Extension ) then
			raise CONSTRAINT_ERROR with "I can't show you my template sources! Sorry!";
		end if;

		-- if it got here, everything went well
		Response := AWS.Response.File(
				Content_Type	=> AWS.MIME.Content_Type( Complete_Path ),
				Filename	=> Complete_Path
			);
	end Process_Custom_Request;


	overriding
	procedure Process_Json_Request(
			Service		: in out Theme_Service;
			Status		: in     Request_Status_Type;
			Response	:    out KOW_Lib.Json.Object_Type
		) is
		-- will raise program_error with nice message
	begin
		raise PROGRAM_ERROR with "can't handle json request in the theme service";
	end Process_Json_Request;

end KOW_View.Themes.Services;
