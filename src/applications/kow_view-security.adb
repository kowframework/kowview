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
with Templates_Parser;

package body KOW_View.Security is

	function Get_User( Request : in AWS.Status.Data ) return KOW_Sec.User_Type is
		-- get the user information using the request...
	begin
		return User_Data.Get( AWS.Status.Session( Request ), User_Key );
	end Get_User;


	procedure Reload_User_Data( Request : in AWS.Status.Data ) is
		-- reload the logged user data into memory...
		-- usefull when updating user settings and such
		User : KOW_Sec.User_Type := Get_user( Request );
	begin
		User.Data := KOW_Sec.Get_User( User.Data.Identity );
		User_Data.Set( AWS.Status.Session( Request ), User_Key, User );
	end Reload_User_Data;


	procedure Insert(
				Params	: in out Templates_Parser.Translate_Set;
				User	: in     KOW_Sec.User_Type
			) is
	begin
		Insert( Params, User.Data );
	end Insert;
	

	procedure Insert(
				Params	: in out Templates_Parser.Translate_Set;
				User	: in     KOW_Sec.User_Data_Type
			) is

		procedure Set( Key, Value : in String ) is
			pragma Inline( Set );
			use Templates_Parser;
		begin
			Insert(
					Params,
					Assoc( Key, Ada.Strings.Fixed.Trim( Value, Ada.Strings.Both ) )
				);
		end Set;
	begin
		Set( "identity",		String( User.Identity ) );
		Set( "account_status",		KOW_Sec.Account_Status_type'Image( User.Account_Status ) );
		Set( "account_status_message",	User.Account_Status_Message );
		Set( "first_name",		User.First_Name );
		Set( "last_name",		User.Last_Name );
		Set( "nickname",		User.Nickname );
		Set( "primary_email",		User.Primary_Email );
	end Insert;



end KOW_View.Security;
