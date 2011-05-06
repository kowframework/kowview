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
pragma License( GPL );


------------------------------------------------------------------------------
-- Main package for the Pages application                                   --
------------------------------------------------------------------------------

--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_View.Modules;
with KOW_View.Modules.Implementations;
with KOW_View.Modules.Stateless_Module_Factories;
with KOW_View.Pages.Components;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Status;

package body KOW_View.Pages.Modules is


	---------------------------
	-- Exception Test Module --
	---------------------------

	
	overriding
	procedure Process_Body(
			Module	: in out Exception_Test_Module;
			Request	: in     AWS.Status.Data;
			Response:    out Ada.Strings.Unbounded.Unbounded_String
		) is
	begin
		raise AN_EXCEPTION with "so you can test the exception page";
	end Process_Body;
end KOW_View.Pages.Modules;
