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
with ada.text_io;




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
with KOW_View.Locales;
with KOW_View.Modules;
with KOW_View.Security.Components;
with KOW_View.Security.REST;

---------
-- AWS --
---------
with AWS.Status;

package body KOW_View.Security.Modules is



	---------------------
	-- Criteria Module --
	---------------------

	overriding
	procedure Initialize_Request(
				Module	: in out Criteria_Module;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			) is
		-- where the magic happens
		Criteria : KOW_Sec.Authorization_Criterias.Expression_Criteria_Type;
	begin
		Criteria.Descriptor := KOW_Config.Element( Config, "descriptor" );

		Add_Contexts(
				Module	=> Criteria_Module'Class( Module ), 
				Criteria=> Criteria,
				Request	=> Request
			);

		KOW_Sec.Accounting.Require( Criteria, KOW_View.Security.Get_User( Request ), Accountant'Access );
	end Initialize_Request;


	--------------------------
	-- Head Criteria Module --
	--------------------------

	overriding
	procedure Initialize_Request(
				Module	: in out Head_Criteria_Module;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			) is
	begin
		Module.Descriptor := KOW_Config.Element( Config, "descriptor" );
	end Initialize_Request;

	
	overriding
	procedure Process_Head(
				Module	: in out Head_Criteria_Module;
				Request	: in     AWS.Status.Data;
				Output	:    out Unbounded_String
			) is
		Criteria : KOW_Sec.Authorization_Criterias.Expression_Criteria_Type;
	begin
		Criteria.Descriptor := Module.Descriptor;

		Add_Contexts(
				Module	=> Head_Criteria_Module'Class( Module ), 
				Criteria=> Criteria,
				Request	=> Request
			);

		KOW_Sec.Accounting.Require( Criteria, KOW_View.Security.Get_User( Request ), Accountant'Access );
	end Process_Head;


	-----------------------------
	-- Login Controller Module --
	-----------------------------

	HTML : constant String := "html";

	overriding
	procedure Initialize_Request(
				Module	: in out Login_Controller_Module;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			) is
	begin
		Module.Login_Template := KOW_Config.Value( Config, "login_template", Default_Login_Template );
		Module.Logout_Template:= KOW_Config.Value( Config, "logout_template", Default_Logout_Template );
	end Initialize_Request;
	
	overriding
	procedure Process_Head(
				Module	: in out Login_Controller_Module;
				Request	: in     AWS.Status.Data;
				Response:    out Unbounded_String
			) is
		-- get the JavaScript functions for submiting login/logout information
	begin
		ada.text_io.put_line("my body");
		Response := Null_Unbounded_String;
	end Process_Head;


	overriding
	procedure Process_Body(
				Module	: in out Login_Controller_Module;
				Request	: in     AWS.Status.Data;
				Response:    out Unbounded_String
			) is
		-- render the login form/logged user information

		use Templates_Parser;

		User 	: KOW_Sec.User_Type := KOW_View.Security.Get_User( Request );
		Params	: Templates_Parser.Translate_Set;
	begin
		if KOW_Sec.Is_Anonymous( User ) then
			Include_Dojo_Package( Module, "dijit.form.Button" );
			Include_Dojo_Package( Module, "dijit.form.Form" );
			Include_Dojo_Package( Module, "dijit.form.ValidationTextBox" );
			Include_Component_Script( Module, "login.js" );
			KOW_View.Security.REST.Insert_REST_Providers( Params );
			Response := Parse_Template(
							Module			=> Module,
							Template_Resource	=> To_String( Module.Login_Template ),
							Template_Extension	=> HTML,
							Parameters		=> Params,
							Locale			=> KOW_View.Locales.Get_Locale( Request )
						);
		else
			Include_Dojo_Package( Module, "dijit.form.Button" );
			Include_Component_Script( Module, "logout.js" );
			KOW_View.Security.Insert( Params, User );
			Response := Parse_Template(
							Module			=> Module,
							Template_Resource	=> To_String( Module.Logout_Template ),
							Template_Extension	=> HTML,
							Parameters		=> Params,
							Locale			=> KOW_View.Locales.Get_Locale( Request )
						);
		end if;
	end Process_Body;


	---------------------------
	-- Login Required Module --
	---------------------------


	overriding
	procedure Initialize_Request(
				Module	: in out Login_Required_Module;
				Request	: in     AWS.Status.Data;
				Config	: in out KOW_Config.Config_File
			) is
	begin
		if KOW_Sec.Is_Anonymous( KOW_View.Security.Get_user( Request ) ) then
			raise KOW_Sec.Login_Required;
		end if;
	end Initialize_Request;
	

end KOW_View.Security.Modules;
