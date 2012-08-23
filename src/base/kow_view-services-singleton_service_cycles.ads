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
-- Factory implementation for singleton services                          --
------------------------------------------------------------------------------



------------------
-- KOW Famework --
------------------
with KOW_Lib.Json;
with KOW_View.Components;
with KOW_View.Services;

---------
-- AWS --
---------
with AWS.Response;


generic
	type Service_Type is new KOW_View.Services.Service_Type with private;
	Component	: KOW_View.Components.Component_Access;
package KOW_View.Services.Singleton_Service_Cycles is
pragma Elaborate_Body( KOW_View.Services.Singleton_Service_Cycles );


	-------------------
	-- The Factory --
	-------------------

	type Service_Factory_Type is new KOW_View.Components.Service_Factory_Interface with null record;


	overriding
	procedure Process_Json_Request(
			Factory	: in out Service_Factory_Type;
			Status		: in     Request_Status_Type;
			Response	:    out KOW_Lib.Json.Object_Type
		);

	overriding
	procedure Process_Custom_Request(
			Factory	: in out Service_Factory_Type;
			Status		: in     Request_Status_Type;
			Response	:    out AWS.Response.Data
		);


	---------------
	-- Variables --
	---------------

	Service_Instance : Service_Type;
	-- this is the only instance of the service :)
	
	Factory : aliased Service_Factory_Type;

private
	procedure Initialize_Service_Trigger;

end KOW_View.Services.Singleton_Service_Cycles;
