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
pragma License( GPL );


------------------------------------------------------------------------------
-- Services for the Themes application in KOW_View                          --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_View.Services;
with KOW_View.Services.Singleton_Service_Cycles;
with KOW_View.Themes.Components;

---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;


package KOW_View.Themes.Services is

	type Theme_Service is new KOW_View.Services.Service_Type with record
		-- Map a URI to theme resources
		Component_Name		: Unbounded_String;
		Default_Theme_Name	: Unbounded_String;
		Template_Extension	: Unbounded_String;
	end record;


	overriding
	procedure Process_Custom_Request(
			Service		: in out Theme_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		);
	-- process request for a theme's static file
	-- only direct access to files that aren't template are alowed

	overriding
	procedure Process_Json_Request(
			Service		: in out Theme_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out KOW_Lib.Json.Object_Type
		);
	-- will raise program_error with nice message


	package Theme_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
					Service_Type	=> Theme_Service,
					Component	=> KOW_View.Themes.Components.Component'Access
				);

end KOW_View.Themes.Services;
