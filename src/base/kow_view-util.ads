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
-- Utility functions for KOW View                                           --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Tags;


package KOW_View.Util is


	function Get_Type_Name_String(
			Tag	: in Ada.Tags.Tag;
			Sufix	: in String := "_type"
		) return String;
	-- ge the naming component of the tag, giving the following naming conventions
	-- 	Package1.Subpackage1.My_Element_Type
	-- will return (yes, lowercase):
	-- 	my_element 



	function Get_Type_Name(
			Tag	: in Ada.Tags.Tag;
			Sufix	: in String := "_type"
		) return Name_Type;

end KOW_View.Util;
