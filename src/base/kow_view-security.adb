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
pragma License( GPL );


--------------
-- Ada 2005 --
--------------
with Ada.Strings;
with Ada.Strings.Fixed;

-------------------
-- KOW Framework --
-------------------
with KOW_Sec;
with KOW_Sec.Accounting;
with KOW_View;


---------
-- AWS --
---------
with AWS.Session;
with AWS.Status;

package body KOW_View.Security is

	function Is_Anonymous( Request : in AWS.Status.Data ) return Boolean is
		-- check if the user is anonymous or not
	begin
		return KOW_Sec.Is_Anonymous( Get_User( Request ) );
	end Is_Anonymous;



	function Get_User( Request : in AWS.Status.Data ) return KOW_Sec.User_Type is
		-- get the user information using the request...
	begin
		return User_Data.Get( AWS.Status.Session( Request ), User_Key );
	end Get_User;

	procedure Set_User(
				Request	: in AWS.Status.Data;
				User	: in KOW_Sec.User_Type
			) is
		-- set the user
	begin
		User_Data.Set( AWS.Status.Session( Request ), User_Key, User );
	end Set_User;



	procedure Reload_User_Data( Request : in AWS.Status.Data ) is
		-- reload the logged user data into memory...
		-- usefull when updating user settings and such
		User : KOW_Sec.User_Type := Get_user( Request );
	begin
		User.Data := KOW_Sec.Get_User( User.Data.Identity );
		Set_USer( Request, User );
	end Reload_User_Data;


end KOW_View.Security;
