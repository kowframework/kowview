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
-- Modules package for KOW Sec integration in KOW View                      --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Json;
with KOW_Sec.Accounting;
with KOW_View.Modules;
with KOW_View.Modules.Stateless_Module_Factories;
with KOW_View.Security.Components;

---------
-- AWS --
---------
with AWS.Status;

package KOW_View.Security.Modules is


	Accountant : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "modules", KOW_View.Security.Accountant'Access );


	type Login_Controller_Module is new KOW_View.Modules.Module_Type with record
		Username_Label		: Unbounded_String;
		Password_Label		: Unbounded_String;

		Logged_as_Label		: Unbounded_String;
		Logout_Label		: Unbounded_String;

		Login_Template	: Unbounded_String;
		Logout_Template	: Unbounded_String;
	end record;

	overriding
	procedure Initialize_Request(
				Module	: in out Login_Controller_Module;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			);
	
	overriding
	procedure Process_Head(
				Module	: in out Login_Controller_Module;
				Request	: in     AWS.Status.Data;
				Response:    out Unbounded_String
			);
	-- get the JavaScript functions for submiting login/logout information
	
	overriding
	procedure Process_Body(
				Module	: in out Login_Controller_Module;
				Request	: in     AWS.Status.Data;
				Response:    out Unbounded_String
			);
	-- render the login form/logged user information

	overriding
	procedure Process_JSon_Request(
				Module	: in out Login_Controller_Module;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			);
	-- process the login/logout returing a redirect string or error message

	package Login_Controller_Factories is new KOW_View.Modules.Stateless_Module_Factories(
							Module_Type	=> Login_Controller_Module,
							Component	=> KOW_View.Security.Components.Component'Access
						);

private

	Default_Login_Template	: constant Unbounded_String := To_Unbounded_String( "login" );
	Default_Logout_Template	: constant Unbounded_String := To_Unbounded_String( "info" );
	
end KOW_View.Security.Modules;
