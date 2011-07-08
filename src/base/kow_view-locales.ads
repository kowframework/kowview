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


package KOW_View.Locales is



	---------------
	-- Constants --
	---------------
	Session_Key : constant String := "kow_view::locale";


	------------------
	-- Session Data --
	------------------
	package Locale_Data is new AWS.Session.Generic_Data(
					Data		=> KOW_Lib.Locales.Locale,
					Null_Data	=> KOW_Lib.Locales.Get_Default_Locale
				);
	
	
	-------------
	-- Methods --
	-------------

	function Get_Locale( Request : in AWS.Status.Data ) return KOW_Lib.Locales.Locale;
	-- get the session's locale

	procedure Set_Locale(
				Request	: in AWS.Status.Data;
				Locale	: in KOW_Lib.Locales.Locale
			);
	-- set the session's locale

	function Get_Dojo_Locale( Request : in AWS.Status.Data ) return String;
	-- return the locale in the Dojo formatting standard;
	-- ie (ISO => Dojo):
	-- 	pt_BR => pt-br
	-- 	en_US => en-us

end KOW_View.Locales;
