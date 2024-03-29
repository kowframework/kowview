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




--------------
-- Ada 2005 --
--------------
with Ada.Exceptions;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_Sec.Accounting;


---------
-- AWS --
---------
with AWS.Exceptions;
with AWS.Log;
with AWS.Status;
with AWS.Response;


package KOW_View is



	---------
	-- Log --
	---------

	Accountant : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "kow_sec" );



	----------------
	-- Core Types --
	----------------


	--
	-- Request Mode
	--
	type Request_Mode_Type is(
			Json_Request,
			Custom_Request
		);


	-- 
	-- Names 
	--

	type Name_Type is new String( 1 .. 150 );

	No_Name : constant Name_Type := ( others => ' ' );


	function To_Name( Str : in String ) return Name_Type;
	-- convert any-lenghted str to Name_Type (if str'length > Name_Type'length raises constraint error)

	function To_String( Path : in Name_Type ) return String;
	-- convert the path into a trimmed string

	subtype Component_Name is Name_Type;	-- @see KOW_View.Components
	subtype Service_Name   is Name_Type;	-- @see KOW_View.Services
	subtype Context_Name   is Name_Type;	-- to be used by the service implementation


	No_Component : constant Component_Name := ( others => ' ' );
	No_Service   : constant Service_Name   := ( others => ' ' );
	No_Context   : constant Context_Name   := ( others => ' ' );


	-- 
	-- Request Status
	--
	type Request_Status_Type is record
		Mode			: Request_Mode_Type;
		-- if it's a json a custom request
		Mapped_URI		: Name_Type;
		-- the part of the URI that has been mapped 
		Mapped_Expression	: Name_Type;
		-- the expression used to map this URI

		Local_URI		: Name_Type;
		-- the part of the URI that hasn't been mapped

		Request_Parameters	: KOW_Lib.Json.Object_Type;
		-- parameters that are built for this request
		-- the request dispatcher is responsible for initializing this attribute
		-- it can contain anything (but usually contains parameters extracted from the URI)

		Request			: AWS.Status.Data;
		-- the AWS.Status.Data value for the given request


		Context			: Context_Name   := No_Context;
		-- the context is an arbitrary HTTP parameter that's
		-- set by the dispatcher and used by the service
		--
		--
		-- in page services, it will identify the module ID for json
		-- requests for instance
	end record;



	-----------------------
	-- Useful processors --
	-----------------------

	function Process_Login_Required( Request : in AWS.Status.Data ) return AWS.Response.Data;
	-- redirect to the login page
	


	-----------------------
	-- Default Processor --
	-----------------------



	function Process_Request( Request : in AWS.Status.Data ) return AWS.Response.Data;
	-- this is the main function... it's the AWS callback used all around.
	-- notice that in the v2.0 release the package KOW_View.Service_Mappings was extinguished
	--
	-- Also treat some default exceptions:
	-- This procedure behaves in the following way:
	-- 	when REDIRECT_TO_HOME	=> redirect to the home page :)
	-- 	when LOGIN_REQUIRED	=> redirect to the login page :)


	procedure Handle_Exception(
				E	: in     Ada.Exceptions.Exception_Occurrence;
				Log	: in out AWS.Log.Object;
				Error	: in     AWS.Exceptions.Data;
				Answer	: in out AWS.Response.Data
			);
	-- default exception handler, should be set called:
	-- 	AWS.Server.Set_Unexpected_Exception_Handler
	--
	-- Given that the exeption page will be stored in:
	-- 	./data/exceptions/package.otherpackage.exception	=> page for the given exception
	-- 	./data/exceptions/others.html				=> fallback
	--


	REDIRECT : Exception;
	-- whenever you need you can raise this exception to get your application redirected somewhere
	-- example:	raise REDIRECT with "http://www.google.com";

	REDIRECT_TO_HOME : Exception;
	-- redirect to the home for this server

	ERROR_404 : Exception;
	-- the "not found" HTTP error code
	

	Home		: Unbounded_String := To_Unbounded_String( "/" );
	-- a string representing the main page for this server :)
	-- default is / but can be overriden

	Login_Page		: Unbounded_String := To_Unbounded_String( "/security/login" );
	-- the URI for the login page


	E_Mail_On_Exceptions	: Boolean := False;
	-- send email with information about excetpions that aren't expected
	-- disabled by default on development environments

	E_Mail_From_Name	: Unbounded_String := To_Unbounded_String( "KOW" );
	E_Mail_From_Address	: Unbounded_String := To_Unbounded_String( "contato@kow.com.br" );

	E_Mail_To_Name		: Unbounded_String := To_Unbounded_String( "Marcelo" );
	E_Mail_To_Address	: Unbounded_String := To_Unbounded_String( "marcelo@kow.com.br" );

	Error_E_Mail_Subject	: Unbounded_String := To_Unbounded_String( "[Unhandled Exception] " );

	E_Mail_SMTP_Server	: Unbounded_String := To_Unbounded_String( "localhost" );



	------------------------------
	-- Email Sending Procedures --
	------------------------------

	procedure Send_Email(
			To_Name		: in String;
			To_Address	: in String;
			Subject		: in String;
			Message		: in String
		);



	procedure Send_Email(
			To	: in KOW_Sec.User_Data_Type;
			Subject	: in String;
			Message	: in String
		);
	-- send plain text email messages


	subtype Virtual_Host_Name_Type is String( 1 .. 100 );
	
	Enable_Virtual_Host	: Boolean := False;
	-- enable name virtual hosts
	-- TODO :: implement virtual hosts
	-- it should map several domain names into a single virtual hosts (most problably using regexp)

	function Virtual_Host(
			Status	: in Request_Status_Type
		) return Virtual_Host_Name_Type;
	-- compute the current virtual host

end KOW_View;
