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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Utility functions for KOW View                                           --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Tags;



package body KOW_View.Util is


	function Get_Type_Name( Tag : Ada.Tags.Tag ) return String is
		-- ge the naming component of the tag, giving the following naming conventions
		-- 	Package1.Subpackage1.My_Element_Type
		-- will return:
		-- 	my_element (yes, lowercase)
		use Ada.Strings;
		T	: constant String := Ada.Tags.Expanded_name( Tag );
		First	: constant String := Fixed.Index( T, ".", Backward );
		Last	: constant Integer := T'Last - 5;
	begin
		if First < 0 then
			-- it's not inside any package.. good
			T := T'First;
		else
			First := First + 1;
		end if;

		return Ada.Characters.Handling.To_Lower( T( First .. Last ) );
	end Get_Type_Name;



	function Get_Type_Name( Tag : Ada.Tags.Tag ) return Ada.Strings.Unbounded.Unbounded_String is
	begin
		return Ada.Strings.Unbounded.To_Unbounded_String( Get_Type_name( Tag ) );
	end Get_Type_Name;
end KOW_View.Util;
