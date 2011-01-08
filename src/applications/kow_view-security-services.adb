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
-- Package for security services                                            --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Strings;
with Ada.Strings.Fixed;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_Sec;
with KOW_View.Components;
with KOW_View.Locales;
with KOW_View.Security.Components;
with KOW_View.Security.REST;

---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;
with AWS.Response.Set;
with AWS.Session;
with AWS.Status;
with Templates_Parser;

package body KOW_View.Security.Services is



	--------------------
	-- Helper methods --
	--------------------

	procedure Insert_REST_Providers(
				P 		: in out Templates_Parser.Translate_Set
			) is
		use Templates_Parser;
		use KOW_View.Security.REST;

		REST_Links_Tag	: Tag;
		REST_Labels_Tag	: Tag;
		REST_Icons_Tag	: Tag;

		Providers : REST_Login_Provider_Vectors.Vector := Get_Providers;

		procedure Iterator( C : in REST_Login_Provider_Vectors.Cursor ) is
			Provider : Rest_Login_Provider_Type := REST_Login_Provider_Vectors.Element( C );
		begin
			REST_Links_Tag	:= REST_Links_Tag	& Get_Link( Provider );
			REST_Labels_Tag	:= REST_Labels_Tag	& Provider.Label;
			REST_Icons_Tag	:= REST_Icons_Tag	& Get_Icon( Provider, Big_Icon );
		end Iterator;
	begin
		REST_Login_Provider_Vectors.Iterate( Providers, Iterator'Access );

		Insert( P, Assoc( "REST_links", REST_Links_Tag ) );
		Insert( P, Assoc( "REST_labels", REST_Labels_Tag ) );
		Insert( P, Assoc( "REST_icons", REST_Icons_Tag ) );

	end Insert_Rest_Providers;

	-------------------
	-- Login Service --
	-------------------


	overriding
	procedure Process_Custom_Request(
				Service		: in out Login_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out AWS.Response.Data
			) is
		-- if username and password are provided, try the login process...
		--	 if success try redirecting to:
		--	 	if 'redirect' parameter exists, go there
		--		or else, if component.default_redirect is set, go there
		--		or else, redirect to home
		--	if error, show the login_error page
		-- else
		-- 	show the login page
	
		P		: constant AWS.Parameters.List := AWS.Status.Parameters( Request );
		Session_ID	: constant AWS.Session.ID      := AWS.Status.Session( Request );
	
		Username	: constant String := AWS.Parameters.Get( P, "username" );
		Password	: constant String := AWS.Parameters.Get( P, "password" );
		Redirect	: constant String := AWS.Parameters.Get( P, "redirect" );


		Params : Templates_Parser.Translate_set;
		-- to be used in case of fail or even rendering the login page..
	begin

		if Username = "" and then Password = "" then
			Insert_REST_Providers( Params );
			Response := AWS.Response.Build(
						"text/html",
						Parse_Template(
								Service			=> Service,
								Template_Resource	=> Login_Page,
								Template_Extension	=> HTML,
								Parameters		=> Params,
								Locale			=> KOW_View.Locales.Get_Locale( Request )
							)
					);
		else


			KOW_View.Security.User_Data.Set(
						Session_Id,
						User_Key,
						KOW_Sec.Do_Login( Username, Password )
					);


			if Redirect /= "" then
				raise KOW_View.REDIRECT with Redirect;
			elsif KOW_View.Security.Components.Default_Redirect_URI /= "" then
				raise KOW_View.REDIRECT with To_String( KOW_View.Security.Components.Default_Redirect_URI );
			else
				raise KOW_View.REDIRECT_TO_HOME;
			end if;
		end if;

	exception
		when e : others =>
			declare	
				use Templates_Parser;
			begin
				Insert_REST_Providers( Params );
				Response := AWS.Response.Build(
							"text/html",
							Parse_Template(
									Service			=> Service,
									Template_Resource	=> Login_Error_Page,
									Template_Extension	=> HTML,
									Parameters		=> Params,
									Locale			=> KOW_View.Locales.Get_Locale( Request )
								)
							);
			end;
	end Process_Custom_Request;

	overriding
	procedure Process_Json_Request(
				Service		: in out Login_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out KOW_Lib.Json.Object_Type
			) is
		-- tries the login process returning the user information on success 
		-- raises exception on errror (and the framework will gently send it back to the caller)

		P		: constant AWS.Parameters.List := AWS.Status.Parameters( Request );
		Session_ID	: constant AWS.Session.ID      := AWS.Status.Session( Request );
	
		Username	: constant String := AWS.Parameters.Get( P, "username" );
		Password	: constant String := AWS.Parameters.Get( P, "password" );


		Params : Templates_Parser.Translate_set;
		-- to be used in case of fail or even rendering the login page..


		Object		: KOW_Lib.Json.Object_Type;
		User		: KOW_Sec.User_Type;
	begin

		if Username = "" and then Password = "" then
			raise KOW_Sec.Invalid_Credentials with "no information supplied";
		end if;

		User := KOW_Sec.Do_Login( Username, Password );
		KOW_View.Security.User_Data.Set(
					Session_Id,
					User_Key,
					User
				);
		KOW_Lib.Json.Set( Object, "user", KOW_Sec.To_Json( User.Data ) );

		Response := Object;
	end Process_Json_Request;



	--------------------
	-- Logout Service --
	--------------------



	overriding
	procedure Process_Custom_Request(
				Service		: in out Logout_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out AWS.Response.Data
			) is
		-- simply logouts and redirects to /
		-- if there is no user logged in no error is displayed

		Session_ID	: constant AWS.Session.ID      := AWS.Status.Session( Request );
		Resp		: AWS.Response.Data := AWS.Response.URL ( "/" );
	begin

		AWS.Session.Delete( Session_ID );
		AWS.Response.Set.Clear_Session( Resp );

		Response := Resp;
	end Process_Custom_Request;


	overriding
	procedure Process_Json_Request(
				Service		: in out Logout_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out KOW_Lib.Json.Object_Type
			) is
	begin
		raise CONSTRAINT_ERROR with "no json logout";
	end Process_Json_Request;



	-----------------------
	-- User Info Service --
	-----------------------



	overriding
	procedure Process_Custom_Request(
				Service		: in out User_Info_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out AWS.Response.Data
			) is
		-- show a simple user information html page
	
		use KOW_Sec;
		use Templates_Parser;
	
		User	: User_Data_Type := Get_User( Request ).Data;
		Params	: Translate_Set;
		procedure Set( Key, Value : in String ) is
			pragma Inline( Set );
		begin
			Insert(
					Params,
					Assoc( Key, Ada.Strings.Fixed.Trim( Value, Ada.Strings.Both ) )
				);
		end Set;
	begin
		Set( "identity",		String( User.Identity ) );
		Set( "account_status",		Account_Status_type'Image( User.Account_Status ) );
		Set( "account_status_message",	User.Account_Status_Message );
		Set( "first_name",		User.First_Name );
		Set( "last_name",		User.Last_Name );
		Set( "nickname",		User.Nickname );
		Set( "primary_email",		User.Primary_Email );

		Response := AWS.Response.Build(
					"text/html",
					Parse_Template(
							Service			=> Service,
							Template_Resource	=> User_Info_Page,
							Template_Extension	=> HTML,
							Parameters		=> Params,
							Locale			=> KOW_View.Locales.Get_Locale( Request )
						)
					);

	end Process_Custom_Request;


	overriding
	procedure Process_Json_Request(
				Service		: in out User_Info_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out KOW_Lib.Json.Object_Type
			) is
		-- return a json object with user: to_json(loged_user);
		Object : KOW_Lib.Json.Object_Type;
	begin
		KOW_Lib.Json.Set( Object, "user", KOW_Sec.To_Json( KOW_View.Security.Get_user( Request ).Data ) );
		Response := Object;
	end Process_Json_Request;
end KOW_View.Security.Services;
