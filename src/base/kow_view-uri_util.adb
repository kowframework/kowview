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

	Page_Protocol : constant String := "page://";

	function Is_Page_URL( URL : in String ) return Boolean is
		-- check if it's page://
	begin
		return URL'Length > Page_Protocol'Length and then URL( URL'First .. URL'First + Page_Protocol'Length - 1 ) = Page_Protocol;
	end Is_Page_URL;


	function Get_Page_Name( URL : in String ) return String is
		-- return the page name from the page:// URL (starting with forward slash)
	begin
		if not Is_Page_URL( URL ) then
			raise CONSTRAINT_ERROR with "can't get page name from URL " & URL;
		end if;

		return URL( URL'First + Page_protocol'Length .. URL'Last );
	end Get_Page_Name;


	function To_Page_URI( URL : in String ) return String is
	-- convert the given page:// URL into an URI that can be used to access a page
	begin
		return "/pages/page/" & Get_Page_Name( URL );
	end To_Page_URI;

end KOW_View.URI_Util;
