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
-- Package for security services                                            --
------------------------------------------------------------------------------



-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_View.Components;
with KOW_View.Security.Components;
with KOW_View.Services.Singleton_Service_Cycles;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Status;

package KOW_View.Security.Services is

	---------------
	-- Constants --
	---------------


	Login_Page	: constant String := "login_page";
	Login_Error_Page: constant String := "login_error_page";

	HTML		: constant String := "html";			-- to avoid typo error

	User_Info_Page	: constant String := "user_info";


	-------------------
	-- Login Service --
	-------------------

	type Login_Service is new KOW_View.Services.Service_Type with null record;
	-- validates user login input (username/password) 
	
	overriding
	procedure Process_Custom_Request(
				Service		: in out Login_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out AWS.Response.Data
			);
	-- if username and password are provided, try the login process...
	--	 if success try redirecting to:
	--	 	if 'redirect' parameter exists, go there
	--		or else, if component.default_redirect is set, go there
	--		or else, redirect to home
	--	if error, show the login_error page
	-- else
	-- 	show the login page
	
	overriding
	procedure Process_Json_Request(
				Service		: in out Login_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out KOW_Lib.Json.Object_Type
			);
	-- tries the login process returning the user information on success 
	-- raises exception on errror (and the framework will gently send it back to the caller)

	package Login_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
					Service_Type	=> Login_Service,
					Component	=> KOW_View.Security.Components.Component'Access
				);



	--------------------
	-- Logout Service --
	--------------------


	type Logout_Service is new KOW_View.Services.Service_Type with null record;
	-- logout the user redirecting to the "redirect" HTTP parameter
	-- supports both json and custom calls


	overriding
	procedure Process_Custom_Request(
				Service		: in out Logout_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out AWS.Response.Data
			);
	-- simply logouts and redirects to /
	-- if there is no user logged in no error is displayed


	overriding
	procedure Process_Json_Request(
				Service		: in out Logout_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out KOW_Lib.Json.Object_Type
			);
	-- raise CONSTRAINT_ERROR...
	--
	-- the logout has got to be via custom request because we need to clear the session data
	-- and send the browser command to delete S_ID.
	--
	-- thus we need to change the AWS response object

	package Logout_Service_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
					Service_Type	=> Logout_Service,
					Component	=> KOW_View.Security.Components.Component'Access
				);


	-----------------------
	-- User Info Service --
	-----------------------

	type User_Info_Service is new KOW_View.Services.Service_Type with null record;
	-- display information about the user..
	-- TODO :: see if it's a good idea to extend this to a more powerfull quering service

	overriding
	procedure Process_Custom_Request(
				Service		: in out User_Info_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out AWS.Response.Data
			);
	-- show a simple user information html page

	overriding
	procedure Process_Json_Request(
				Service		: in out User_Info_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out KOW_Lib.Json.Object_Type
			);
	-- return a json object with user: to_json(loged_user);

	package User_Info_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
					Service_Type	=> User_Info_Service,
					Component	=> KOW_View.Security.Components.Component'Access
			);

	
	-------------------------
	-- Switch User Service --
	-------------------------



	type Switch_User_Service is new KOW_View.Services.Service_Type with null record;
	-- just like "su" for root users :)
	--
	-- the active user can't be anonymous and must have the kow_view.security.switch_user role


	overriding
	procedure Process_Custom_Request(
				Service		: in out Switch_User_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out AWS.Response.Data
			);
	
	overriding
	procedure Process_Json_Request(
				Service		: in out Switch_User_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out KOW_Lib.Json.Object_Type
			);
	
	package Switch_User_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
					Service_Type	=> Switch_User_Service,
					Component	=> KOW_View.Security.Components.Component'Access
				);

end KOW_View.Security.Services;
