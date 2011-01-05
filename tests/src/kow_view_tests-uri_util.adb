-----------------------------------------------------------------------------
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


-----------
-- Ahven --
-----------
with Ahven;
with Ahven.Framework;



-------------------
-- KOW Framework --
-------------------
with KOW_View.URI_Util;		use KOW_View.URI_Util;

package body KOW_View_Tests.URI_Util is


	overriding
	procedure Initialize( T : in out Test_Type ) is
	begin
		Set_Name( T, "KOW_View.URI_Util" );
		Ahven.Framework.Add_Test_Routine( T, Test_Is_Page_URN'Access, "Is_Page_URN" );
		Ahven.Framework.Add_Test_Routine( T, Test_Get_Page_Name'Access, "Get_Page_Name" );
		Ahven.Framework.Add_Test_Routine( T, Test_To_Page_URI'Access, "To_Page_URI" );
	end Initialize;



	Page_Name	: constant String := "some_page/at_some_place";
	Page_URN	: constant String := "page:" & Page_Name;
	Page_URI	: constant String := "/pages/page/" & Page_Name;



	procedure Test_Is_Page_URN is
	begin
		Ahven.Assert(
				Condition	=> Is_Page_URN( Page_URN ),
				Message		=> "Failed validating valid page URN"
			);
		Ahven.Assert(
				Condition	=> not Is_Page_URN( "http://framework.kow.com.br" ),
				Message		=> "Failed validation invalid page URN"
			);
	end Test_Is_Page_URN;


	procedure Test_Get_Page_Name is
	begin
		Ahven.Assert(
				Condition	=> Page_Name = Get_Page_Name( Page_URN ),
				Message		=> "Can't get page name from URN :: expected """ & Page_Name & """ received """ & Get_Page_Name( Page_URN ) & """"
			);
	end Test_Get_Page_Name;


	procedure Test_To_Page_URI is
	begin
		Ahven.Assert(
				Condition	=> Page_URI = To_Page_URI( Page_URN ),
				Message		=> "Can't get page URI from URN :: expected """ & Page_URI & """ received """ & To_Page_URI( Page_URN ) & """"
			);
	end Test_To_Page_URI;

end KOW_View_Tests.URI_Util;
