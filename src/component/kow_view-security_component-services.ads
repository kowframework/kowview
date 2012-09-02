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



-------------------
-- KOW Framework --
-------------------
with KOW_View.Components;				use KOW_View.Components;
with KOW_View.Services;
with KOW_View.Services.Singleton_Service_Factories;


package KOW_View.Security_Component.Services is


	-------------------
	-- Login Service --
	-------------------

	type Login_Service is new KOW_View.Services.Service_Type with null record;

	overriding
	procedure Process_Custom_Request(
				Service	: in out Login_Service;
				Status	: in     Request_Status_Type;
				Response:    out AWS.Response.Data
			);
	-- do login and redirect to the referer OR destination parameter


	overriding
	procedure Process_Json_Request(
				Service	: in out Login_Service;
				Status	: in     Request_Status_Type;
				Response:    out KOW_Lib.Json.Object_Type
			);
	-- do login fi needed, and print user info


	package Login_Factories is new KOW_View.Services.Singleton_Service_Factories(
								Service_Type	=> Login_Service,
								Component	=> Component
							);
end KOW_View.Security_Component.Services;
