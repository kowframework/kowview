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
with KOW_Sec.Accounting;


---------
-- AWS --
---------
with AWS.Exceptions;
with AWS.Log;
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

	
	type Welcome_Function_Type is access function( Request : in AWS.Status.Data ) return AWS.Response.Data;
	function Default_Welcome_Function( Request : in AWS.Status.Data ) return AWS.Response.Data;
	-- redirects to home

	REDIRECT : Exception;
	-- whenever you need you can raise this exception to get your application redirected somewhere
	-- example:	raise REDIRECT with "http://www.google.com";

	REDIRECT_TO_HOME : Exception;
	-- redirect to the home for this server


	Home		: Unbounded_String := To_Unbounded_String( "/pages/page" );
	-- a string representing the main service.. :)
	-- default is the page component, but can be overriden

	Login_Page		: Unbounded_String := To_Unbounded_String( "/security/login" );
	-- the URI for the login page


	E_Mail_On_Exceptions	: Boolean := False;
	-- send email with information about excetpions that aren't expected
	-- disabled by default on development environments

	E_Mail_From_Name	: Unbounded_String := To_Unbounded_String( "Teca" );
	E_Mail_From_Address	: Unbounded_String := To_Unbounded_String( "teca@teca.etc.br" );

	E_Mail_To_Name		: Unbounded_String := To_Unbounded_String( "Marcelo" );
	E_Mail_To_Address	: Unbounded_String := To_Unbounded_String( "marcelo@kow.com.br" );

	Error_E_Mail_Subject	: Unbounded_String := To_Unbounded_String( "[Unhandled Exception] " );

	E_Mail_SMTP_Server	: Unbounded_String := To_Unbounded_String( "localhost" );


	Welcome_Function	: Welcome_Function_Type := Default_Welcome_Function'Access;
	-- the processor for when the URI is /
	-- the default behaviour (if it's null) / is to redirect to home.



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

end KOW_View;
