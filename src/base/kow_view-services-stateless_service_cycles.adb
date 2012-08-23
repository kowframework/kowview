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

------------------------------------------------------------------------------
-- Factory implementation for Stateless services                          --
------------------------------------------------------------------------------



------------------
-- KOW Famework --
------------------
with KOW_Lib.Json;
with KOW_View.Components;
with KOW_View.Services.Util;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Session;


package body KOW_View.Services.Stateless_Service_Cycles is

	-------------------
	-- The Factory --
	-------------------


	overriding
	procedure Process_Json_Request(
			Factory	: in out Service_Factory_Type;
			Status		: in     Request_Status_Type;
			Response	:    out KOW_Lib.Json.Object_Type
		) is
		Service : Service_Type;
	begin
		Setup_Service( Component, Service );

		Process_Json_Request(
				Service	=> Service,
				Status	=> Status,
				Response=> Response
			);
	end Process_Json_Request;


	overriding
	procedure Process_Custom_Request(
			Factory	: in out Service_Factory_Type;
			Status		: in     Request_Status_Type;
			Response	:    out AWS.Response.Data
		) is
		Service : Service_Type;
	begin
		Setup_Service( Component, Service );
		Process_Custom_Request(
				Service	=> Service,
				Status	=> Status,
				Response=> Response
			);
	end Process_Custom_Request;


begin
	-------------------------------
	-- we register the Factory --
	-------------------------------
	KOW_View.Components.Register_Service_Factory(
				Component.all,
				KOW_View.Services.Util.Get_Name( Service_Type'Tag ),
				Factory'Unrestricted_Access
			);
end KOW_View.Services.Stateless_Service_Cycles;
