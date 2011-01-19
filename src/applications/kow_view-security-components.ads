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
-- Components package for KOW Sec integration in KOW View                   --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- The security inside KOW View is managed by KOW Sec. It's  this in every  --
-- layer.                                                                   --
--                                                                          --
-- This is a higher level layer with some modules and services for handling --
-- some aspects of the application security, such as login                  --
------------------------------------------------------------------------------

--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Sec.Accounting;
with KOW_View.Components;


package KOW_View.Security.Components is
	Accountant : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "components", KOW_View.Security.Accountant'Access );



	type Security_Component is new KOW_View.Components.Component_Type with null record;
	Default_Redirect_URI	: Unbounded_String;

	overriding
	procedure Setup(
			Component	: in out Security_Component;
			Config		: in out KOW_Config.Config_File
		);
	-- the setup expects for the values of:
	-- 	default_redirect	=> where to redirect when the login call doesn't specify (empty causes to redirect to home)
	-- 	access_denied		=> where to redirect when the access has been denied
	-- 	login_required		=> where to redirect when the user needs to be logged in to proceed
	--
	-- Both can be any kind of URI (including URL and URN).
	-- See KOW_View.URI_Util for more details


	Component : aliased Security_Component;


end KOW_View.Security.Components;
