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
with Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_Sec;
with KOW_Sec.Authorization_Criterias;
with KOW_View.Components;
with KOW_View.Locales;
with KOW_View.Security.Components;
with KOW_View.Security.REST;

---------
-- AWS --
---------
with AWS.Messages;
with AWS.Parameters;
with AWS.Response;
with AWS.Response.Set;
with AWS.Session;
with AWS.Status;
with Templates_Parser;

package body KOW_View.Security.Services is



	-------------------
	-- Login Service --
	-------------------


	overriding
	procedure Process_Custom_Request(
				Service		: in out Login_Service;
				Status		: in     Request_Status_Type;
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
	
		P		: constant AWS.Parameters.List := AWS.Status.Parameters( Status.Request );
		Session_ID	: constant AWS.Session.ID      := AWS.Status.Session( Status.Request );
	
		Username	: constant String := AWS.Parameters.Get( P, "username" );
		Password	: constant String := AWS.Parameters.Get( P, "password" );
		Redirect	: constant String := AWS.Parameters.Get( P, "redirect" );


		Params : Templates_Parser.Translate_set;
		-- to be used in case of fail or even rendering the login page..
	begin

		if Username = "" and then Password = "" then
			KOW_View.Security.REST.Insert_REST_Providers( Params );
			Response := AWS.Response.Build(
						Content_Type	=> "text/html",
						Message_Body	=> Parse_Template(
										Service			=> Service,
										Template_Resource	=> Login_Page,
										Template_Extension	=> HTML,
										Virtual_Host		=> KOW_View.Virtual_Host( Status.Request ),
										Parameters		=> Params,
										Locale			=> KOW_View.Locales.Get_Locale( Status.Request )
									)
--						Encoding	=> AWS.Messages.Deflate
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
		when e : KOW_View.REDIRECT | KOW_View.REDIRECT_TO_HOME =>
			Ada.Exceptions.Reraise_Occurrence( e );
		when e : others =>
			declare	
				use Templates_Parser;
			begin
				KOW_View.Security.REST.Insert_REST_Providers( Params );

				Insert( Params, Assoc( "exception_name", Ada.Exceptions.Exception_Name( e ) ) );
				Insert( Params, Assoc( "exception_message", Ada.Exceptions.Exception_Message( e ) ) );
				Insert( Params, Assoc( "exception_information", Ada.Exceptions.Exception_Information( e ) ) );
				
				Response := AWS.Response.Build(
							"text/html",
							Parse_Template(
									Service			=> Service,
									Template_Resource	=> Login_Error_Page,
									Template_Extension	=> HTML,
									Virtual_Host		=> KOW_View.Virtual_Host( Status.Request ),
									Parameters		=> Params,
									Locale			=> KOW_View.Locales.Get_Locale( Status.Request )
								)
							);
			end;
	end Process_Custom_Request;

	overriding
	procedure Process_Json_Request(
				Service		: in out Login_Service;
				Status		: in     Request_Status_Type;
				Response	:    out KOW_Lib.Json.Object_Type
			) is
		-- tries the login process returning the user information on success 
		-- raises exception on errror (and the framework will gently send it back to the caller)

		P		: constant AWS.Parameters.List := AWS.Status.Parameters( Status.Request );
		Session_ID	: constant AWS.Session.ID      := AWS.Status.Session( Status.Request );
	
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
				Status		: in     Request_Status_Type;
				Response	:    out AWS.Response.Data
			) is
		-- simply logouts and redirects to /
		-- if there is no user logged in no error is displayed

		Session_ID	: constant AWS.Session.ID      := AWS.Status.Session( Status.Request );
		Resp		: AWS.Response.Data := AWS.Response.URL ( "/" );
	begin

		AWS.Session.Delete( Session_ID );
		AWS.Response.Set.Clear_Session( Resp );

		Response := Resp;
	end Process_Custom_Request;


	overriding
	procedure Process_Json_Request(
				Service		: in out Logout_Service;
				Status		: in     Request_Status_Type;
				Response	:    out KOW_Lib.Json.Object_Type
			) is
		Session_ID	: constant AWS.Session.ID      := AWS.Status.Session( Status.Request );
		Resp		: AWS.Response.Data := AWS.Response.URL ( "/" );
		Obj		: KOW_Lib.Json.Object_Type;
	begin

		AWS.Session.Delete( Session_ID );
		AWS.Response.Set.Clear_Session( Resp );

		Response := Obj;
	end Process_Json_Request;



	-----------------------
	-- User Info Service --
	-----------------------



	overriding
	procedure Process_Custom_Request(
				Service		: in out User_Info_Service;
				Status		: in     Request_Status_Type;
				Response	:    out AWS.Response.Data
			) is
		-- show a simple user information html page
	
		use KOW_Sec;
		use Templates_Parser;
	
		User	: User_Data_Type := Get_User( Status.Request ).Data;
		Params	: Translate_Set;
	begin
		Insert( Params, User );

		Response := AWS.Response.Build(
					"text/html",
					Parse_Template(
							Service			=> Service,
							Template_Resource	=> User_Info_Page,
							Template_Extension	=> HTML,
							Virtual_Host		=> KOW_View.Virtual_Host( Status.Request ),
							Parameters		=> Params,
							Locale			=> KOW_View.Locales.Get_Locale( Status.Request )
						)
					);

	end Process_Custom_Request;


	overriding
	procedure Process_Json_Request(
				Service		: in out User_Info_Service;
				Status		: in     Request_Status_Type;
				Response	:    out KOW_Lib.Json.Object_Type
			) is
		-- return a json object with user: to_json(loged_user);
		Object : KOW_Lib.Json.Object_Type;
	begin
		KOW_Lib.Json.Set( Object, "user", KOW_Sec.To_Json( KOW_View.Security.Get_user( Status.Request ).Data ) );
		Response := Object;
	end Process_Json_Request;


	-------------------------
	-- Switch User Service --
	-------------------------

	procedure Do_Switch(
				Service	: Switch_User_Service;
				Status	: Request_Status_Type
			) is
		Criteria	: KOW_Sec.Authorization_Criterias.Role_Criteria_Type;
		Session_ID	: constant AWS.Session.ID      := AWS.Status.Session( Status.Request );


		function Identity return KOW_Sec.User_Identity_Type is
		begin
			return KOW_Sec.To_Identity( To_String( Status.Local_URI ) );-- TODO :: get the identity from the url
		end Identity;
	begin
		Criteria.Descriptor := Ada.Strings.Unbounded.To_Unbounded_String( String( KOW_Sec.Identity( Switch_User ) ) );
		KOW_Sec.Accounting.Require( Criteria, KOW_View.Security.Get_User( Status.Request ), Accountant'Access );


		KOW_View.Security.User_Data.Set(
					Session_Id,
					User_Key,
					(
							Data		=> KOW_Sec.Get_User( Identity ),
							Current_Manager => null
						)
				);

		raise KOW_View.Redirect_To_Home;
	end Do_Switch;


	overriding
	procedure Process_Custom_Request(
				Service		: in out Switch_User_Service;
				Status		: in     Request_Status_Type;
				Response	:    out AWS.Response.Data
			) is
	begin
		Do_Switch( Service, Status );
	end Process_Custom_Request;
	
	overriding
	procedure Process_Json_Request(
				Service		: in out Switch_User_Service;
				Status		: in     Request_Status_Type;
				Response	:    out KOW_Lib.Json.Object_Type
			) is
	begin
		Do_Switch( Service, Status );
	end Process_Json_Request;


end KOW_View.Security.Services;
