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


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;
with AWS.Status;

-------------------
-- KOW Framework --
-------------------
with KOW_View.Components;	use KOW_View.Components;
with KOW_View.Security;
with KOW_View.Services;


package body KOW_View.Security_Component.Services is


	-------------------
	-- Login Service --
	-------------------


	overriding
	procedure Process_Custom_Request(
				Service	: in out Login_Service;
				Status	: in     Request_Status_Type;
				Response:    out AWS.Response.Data
			) is
		-- do login and redirect to the referer OR destination parameter

		use KOW_Sec;
		use KOW_View.Security;

		P : AWS.Parameters.List := AWS.Status.Parameters( Status.Request );

		Username : constant String := AWS.Parameters.Get( P, "username" );
		Password : constant String := AWS.Parameters.Get( P, "password" );
		
		function Destination return String is
			D : constant String := AWS.Parameters.Get( P, "destination" );
			R : constant String := AWS.Status.Referer( Status.Request );
		begin
			if D /= "" then
				return D;
			elsif R /= "" then
				return R;
			else
				return "/";
			end if;
		end Destination;


		User : User_Type;
	begin
		if Is_Anonymous( Status.Request ) then
			User := Do_Login( Username, Password );
			Set_User( Status.Request, User );
			Response := AWS.Response.URL( Destination );
		else
			raise ACCESS_DENIED with "you are already loged in";
		end if;
	end Process_Custom_Request;

	overriding
	procedure Process_Json_Request(
				Service	: in out Login_Service;
				Status	: in     Request_Status_Type;
				Response:    out KOW_Lib.Json.Object_Type
			) is
		-- do login if needed, and print user info
		use KOW_Sec;
		use KOW_View.Security;

		P : AWS.Parameters.List := AWS.Status.Parameters( Status.Request );

		Username : constant String := AWS.Parameters.Get( P, "username" );
		Password : constant String := AWS.Parameters.Get( P, "password" );


		User : User_Type;
	begin
		User := Get_User( Status.Request );
		if Is_Anonymous( User ) then
			User := Do_Login( Username, Password );
			Set_User( Status.Request, User );
		end if;	

		Response := To_Json( User.Data );
	end Process_Json_Request;

end KOW_View.Security_Component.Services;
