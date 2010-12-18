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



-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Locales;


---------
-- AWS --
---------
with AWS.Session;
with AWS.Status;


package body KOW_View.Locales is

	function Get_Locale( Request : in AWS.Status.Data ) return KOW_Lib.Locales.Locale is
		-- get the session's locale
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		return Locale_Data.Get( Session_ID, Session_Key );
	end Get_Locale;

	procedure Set_Locale(
				Request	: in AWS.Status.Data;
				Locale	: in KOW_Lib.Locales.Locale
			) is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		Locale_Data.Set( Session_ID, Session_Key, Locale );
	end Set_Locale;

end KOW_View.Locales;