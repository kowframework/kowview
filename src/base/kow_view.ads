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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Main package for KOW_View                                                --
------------------------------------------------------------------------------

pragma License (Modified_GPL);




---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;

-------------------
-- KOW Framework --
-------------------
with KOW_Sec.Accounting;

package KOW_View is
	Accountant : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "kow_sec" );

	type Request_Mode_Type is(
			Json_Request,
			Custom_Request
		);



	function Process_Request( Request : in AWS.Status.Data ) return AWS.Response.Data;
	-- this is the main function... it's the AWS callback used all around.
	-- notice that in the v2.0 release the package KOW_View.Service_Mappings was extinguished
end KOW_View;