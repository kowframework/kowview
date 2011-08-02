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



--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.String_Util;
with KOW_View.Components;
with KOW_View.Util;

package body KOW_View.Services.Util is

	function Get_Name( Service_Tag : in Ada.Tags.Tag ) return String is
	begin
		return KOW_View.Util.Get_Type_Name( Service_Tag, "_service" );
	end Get_Name;


	function Local_URI(
				Service	: in KOW_View.Services.Service_Type'Class;
				URI	: in String;
				No_Slash: in Boolean := False
			) return String is
		-- get the URI stripping the service mapping

		use KOW_View.Components;
		Mapping : constant String := '/' & Get_Name( Service.Component.all ) & '/' & Get_Name( Service'Tag );

		First : Integer := Mapping'Length + URI'First;
		Last  : Integer := URI'Last;
	begin
		pragma Assert( Mapping'Length <= URI'Length, "this is not a valid URI for this service.. expect something inside " & Mapping );

		if No_Slash then
			First := First + 1;
		end if;

		while Last >= First and then URI( Last ) = '/' loop
			Last := Last - 1;
		end loop;

		if First > Last then
			return "";
		else
			-- NOTE :: this is slow because relies on unbounded_string AND the implementation of str_replace isn't the smartest either.
			-- TODO :: optimize this code
			return URI( First .. Last );
		end if;
	end Local_URI;

end KOW_View.Services.Util;
