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
-- Main package for KOW_View                                                --
------------------------------------------------------------------------------



with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Sec.Accounting;


---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;


package KOW_View is
	Accountant : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "kow_sec" );

	type Request_Mode_Type is(
			Json_Request,
			Custom_Request
		);



	function Process_Request( Request : in AWS.Status.Data ) return AWS.Response.Data;
	-- this is the main function... it's the AWS callback used all around.
	-- notice that in the v2.0 release the package KOW_View.Service_Mappings was extinguished


	REDIRECT_TO_HOME : Exception;
	-- redirect to the home for this server


	Home	: Unbounded_String := To_Unbounded_String( "/page" );
	-- a string representing the main service.. :)
	-- default is the page component, but can be overriden

end KOW_View;
