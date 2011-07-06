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
-- Time_Zone routines for KOW View                                             --
------------------------------------------------------------------------------




--------------
-- Ada 2005 --
--------------
with Ada.Calendar;
with Ada.Calendar.Time_Zones;

---------
-- AWS --
---------
with AWS.Session;
with AWS.Status;


package body KOW_View.Time_Zones is


	-------------
	-- Methods --
	-------------

	function Get_Time_Zone( Request : in AWS.Status.Data ) return Ada.Calendar.Time_Zones.Time_Offset is
		-- get the session's Time_Zone
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		return Time_Zone_Data.Get( Session_ID, Session_Key );
	end Get_Time_Zone;


	procedure Set_Time_Zone(
				Request		: in AWS.Status.Data;
				Time_Zone	: in Ada.Calendar.Time_Zones.Time_Offset
			) is
		-- set the session's Time_Zone
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		Time_Zone_Data.Set( Session_ID, Session_Key, Time_Zone );
	end Set_Time_Zone;


end KOW_View.Time_Zones;
