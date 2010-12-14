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

------------------------------------------------------------------------------
-- Delegator implementation for Stateless services                          --
------------------------------------------------------------------------------



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


package body KOW_View.Services.Stateless_Service_Cycles is

	-------------------
	-- The Delegator --
	-------------------


	overriding
	procedure Process_Json_Request(
			Delegator	: in out Service_Delegator_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		) is
		Service : Service_Type;
	begin
		Setup_Service( Component, Service );

		Process_Json_Request(
				Service	=> Container.Service,
				Request	=> Request,
				Response=> Response
			);
	end Process_Json_Request;


	overriding
	procedure Process_Custom_Request(
			Delegator	: in out Service_Delegator_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		) is
		Service : Service_Type;
	begin
		Setup_Service( Component, Service );
		Process_Custom_Request(
				Service	=> Container.Service,
				Request	=> Request,
				Response=> Response
			);
	end Process_Custom_Request;


begin
	-------------------------------
	-- we register the delegator --
	-------------------------------
	KOW_View.Components.Register_Service_Delegator(
				Component.all,
				KOW_View.Util.Get_Type_Name( Service_Type'Tag ),
				Delegator'Unrestricted_Access
			);
end KOW_View.Services.Stateless_Service_Cycles;
