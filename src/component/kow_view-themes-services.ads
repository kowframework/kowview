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
pragma License (GPL);

------------------------------------------------------------------------------
-- Main package for Theme Engines Default Service                           --
------------------------------------------------------------------------------


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_View.Services;

package KOW_View.Themes.Services is


	Themes_Prefix	: constant String := "/themes/";

	type Theme_Service is new KOW_View.Services.Service_Type with null record;


	procedure Process_Custom_Request(
			Service		: in out Theme_Service;
			Status		: in     Request_Status_Type;
			Response	:    out AWS.Response.Data
		);
	-- look for a theme resource

	procedure Process_Json_Request(
			Service	: in out Theme_Service;
			Status	: in     Request_Status_Type;
			Response:    out KOW_Lib.Json.Object_Type
		);
	-- raise PROGRAM_ERROR with a nice message

end KOW_View.Themes.Services;
