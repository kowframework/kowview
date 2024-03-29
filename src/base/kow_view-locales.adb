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
-- Locale routines for KOW View                                             --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Locales;
with KOW_Lib.String_Util;


---------
-- AWS --
---------
with AWS.Session;
with AWS.Status;


package body KOW_View.Locales is

	function Get_Locale( Status : in Request_Status_Type ) return KOW_Lib.Locales.Locale_Type is
		-- get the session's locale
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session( Status.Request );
	begin
		return Locale_Data.Get( Session_ID, Session_Key );
	end Get_Locale;

	procedure Set_Locale(
				Status	: in Request_Status_Type;
				Locale	: in KOW_Lib.Locales.Locale_Type
			) is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session( Status.Request );
	begin
		Locale_Data.Set( Session_ID, Session_Key, Locale );
	end Set_Locale;

	function Get_Amdjs_Locale( Status : Request_Status_Type ) return String is
		-- return the locale in the Amdjs formatting standard;
		-- ie (ISO => Amdjs):
		-- 	pt_BR => pt-br
		-- 	en_US => en-us
	begin
		return KOW_Lib.String_Util.Str_Replace(
						From	=> '_',
						To	=> '-', 
						Str	=> Ada.Characters.Handling.To_Lower( KOW_Lib.Locales.To_String( Get_Locale( Status ).Code ) )
					);
	end Get_Amdjs_Locale;
end KOW_View.Locales;
