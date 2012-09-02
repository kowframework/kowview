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


--------------
-- Ada 2005 --
--------------
with Ada.Unchecked_Deallocation;

------------------
-- KOW Famework --
------------------
with KOW_Lib.Json;
with KOW_View.Components;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Session;
with AWS.Status;


package body KOW_View.Services.Stateful_Service_Factories is


	type Service_Access is access all Service_Type;

	procedure Free is new Ada.Unchecked_Deallocation( Object => Service_Type, Name => Service_Access );

	function Get( Request : in AWS.Status.Data ) return Service_Type is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
		Service : Service_Type := Service_Data.Get( Session_ID, Session_Key );
	begin
		Service.Component := Component;
		return Service;
	end Get;

	procedure Set( Request : in AWS.Status.Data; Service : in Service_Type ) is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		Service_Data.Set( Session_ID, Session_Key, Service );
	end Set;




	-------------------
	-- The Factory --
	-------------------

	overriding
	procedure Create(
				Factory	: in out Service_Factory_Type;
				Status	: in     Request_Status_Type;
				Service	:    out Service_Ptr
			) is
		Srv : Service_Access :=  new Service_Type'( Get( Status.Request ) );
	begin
		Service := Service_Ptr( Srv );
	end Create;


	overriding
	procedure Destroy(
				Factory	: in out Service_Factory_Type;
				Status	: in     Request_Status_Type;
				Service	: in out Service_Ptr
			) is
	begin
		Set( Status.Request, Service_Access( Service ).all );
		Free( Service_Access( Service ) );
	end Destroy;




end KOW_View.Services.Stateful_Service_Factories;
