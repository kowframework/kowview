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





package body KOW_View.URI_Util is

	Page_URN_Identifier	: constant String := "page:";
	Page_Service_URI	: constant String := "/pages/page/";


	function Is_Page_URN( URN : in String ) return Boolean is
		-- check if it's page:
	begin
		return URN'Length > Page_URN_Identifier'Length and then URN( URN'First .. URN'First + Page_URN_Identifier'Length - 1 ) = Page_URN_Identifier;
	end Is_Page_URN;


	function Get_Page_Name( URN : in String ) return String is
		-- return the page name from the page:// URN (starting with forward slash)
	begin
		if not Is_Page_URN( URN ) then
			raise CONSTRAINT_ERROR with "can't get page name from URN " & URN;
		end if;

		return URN( URN'First + Page_URN_Identifier'Length .. URN'Last );
	end Get_Page_Name;


	function To_Page_URI( URN : in String ) return String is
	-- convert the given page:// URN into an URI that can be used to access a page
	begin
		return Page_Service_URI & Get_Page_Name( URN );
	end To_Page_URI;


end KOW_View.URI_Util;
