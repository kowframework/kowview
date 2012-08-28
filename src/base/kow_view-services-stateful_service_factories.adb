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
-- Factory implementation for Stateful services                          --
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
with AWS.Status;


package body KOW_View.Services.Stateful_Service_Factories is

	---------------------------
	-- The Service Container --
	---------------------------
	function Get( Request : in AWS.Status.Data ) return Service_Container_Type is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
		Container : Service_Container_Type := Service_Container_Data.Get( Session_ID, Service_Container_Key );
	begin
		if Container.Is_Null then
			Setup_Service( Component, Container.Service );
		end if;

		return Container;
	end Get;

	procedure Set( Request : in AWS.Status.Data; Container : in Service_Container_Type ) is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		Service_Container_Data.Set( Session_ID, Service_Container_Key, Container );
	end Set;



	-------------------
	-- The Factory --
	-------------------


	overriding
	procedure Process_Json_Request(
			Factory	: in out Service_Factory_Type;
			Status		: in     Request_Status_Type;
			Response	:    out KOW_lib.Json.Object_Type
		) is
		Container : Service_Container_Type := Get( Status.Request );
	begin

		Process_Json_Request(
				Service	=> Container.Service,
				Status	=> Status,
				Response=> Response
			);
		Set( Status.Request, Container );
	end Process_Json_Request;


	overriding
	procedure Process_Custom_Request(
			Factory	: in out Service_Factory_Type;
			Status		: in     Request_Status_Type;
			Response	:    out AWS.Response.Data
		) is
		Container : Service_Container_Type := Get( Status.Request );
	begin
		Process_Custom_Request(
				Service	=> Container.Service,
				Status	=> Status,
				Response=> Response
			);
		Set( Status.Request, Container );
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
end KOW_View.Services.Stateful_Service_Factories;
