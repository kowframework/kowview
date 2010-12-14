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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------



-------------------
-- KOW Framework --
-------------------
with KOW_View.Services;			use KOW_View.Services;

---------
-- AWS --
---------


package KOW_View.Security.Services is


	-------------------
	-- Login Service --
	-------------------

	type Login_Service is new Service_Type with record
		-- try to login the user and redirect to the correct status page
		Default_Redirect	: Unbounded_String;
		Login_Error_Page	: Unbounded_String;
	end record;


	
	overriding
	procedure Process_Custom_Request(
			Service		: in out Login_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);

	package Login_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
				Component	=> KOW_View.Security.Component'Unrestricted_Access,,
				Service_Type	=> Login_Service_Type
			);


	--------------------
	-- Logout Service --
	--------------------

	type Logout_Service is new Service_Type with record
		-- process the logout, if required, and redirect to some standard page
		Default_Redirect	: Unbounded_String;
	end record;

	overriding
	procedure Process_Custom_Request(
			Service		: in out Logout_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);

	package Logout_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
				Component	=> KOW_View.Security.Component'Unrestricted_Access,,
				Service_Type	=> Logout_Service_Type
			);


	-------------------------
	-- Switch User Service --
	-------------------------


	type Switch_User_Service is new Service_Type with record
		-- allow admins to become super users
		-- TODO :: turn into a stateful service with logout method for getting back as admin
		Default_Redirect	: Unbounded_String;
		Criteria		: Unbounded_String;
	end record;


	overriding
	procedure Process_Custom_Request(
			Service		: in out Switch_User_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);

	package Switch_User_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
				Component	=> KOW_View.Security.Component'Unrestricted_Access,,
				Service_Type	=> Switch_User_Service_Type
			);




end KOW_View.Security.Services;
