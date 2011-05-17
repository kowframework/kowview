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
pragma License( GPL );


------------------------------------------------------------------------------
-- Package with some helper methods for handling URLs                       --
------------------------------------------------------------------------------



-- examples of page Name, URI and URN
-- 	page name	: some_page/at_some_place		(used by the page service)
-- 	URI		: /pages/page/some_page/at_some_place	(used by your browser)
--	URN		: page:some_page/at_some_place		(used by developers)


---------
-- AWS --
---------
with AWS.Status;

package KOW_View.URI_Util is

	function Is_Page_URN( URN : in String ) return Boolean;
	-- check if it's page:
	

	function Get_Page_Name( URN : in String ) return String;
	-- return the page name from the page: URL (without starting forward slash)

	function To_Page_URI( URN : in String ) return String;
	-- convert the given page: URN into an URI that can be used to access a page


	function Build_URL(
			Request		: in AWS.Status.Data;
			Key1,Value1	: in String;
			Key2,Value2	: in String := "";
			Key3,Value3	: in String := "";
			Key4,Value4	: in String := "";
			Key5,Value5	: in String := "";
			Include_URI	: in Boolean := False
		) return String;
	-- build a URL for the current page replacing the HTTP parameters listed in key/value pairs
	-- maintain all other urls
	--
	-- acts like AWS.Parameters.URI_Format BUT replace the existing values by the new given ones

end KOW_View.URI_Util;
