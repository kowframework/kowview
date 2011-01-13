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

------------------------------------------------------------------------------
-- Main package for KOW_View                                                --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.Json;
with KOW_Lib.String_Util;
with KOW_Sec;
with KOW_Sec.Accounting;
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Components.Registry;
with KOW_View.Json_Util;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;
with AWS.SMTP;
with AWS.SMTP.Client;
with AWS.Status;
with Templates_Parser;


package body KOW_View is



	function Process_Request( Request : in AWS.Status.Data ) return AWS.Response.Data is
		-- this is the main function... it's the AWS callback used all around.
		-- notice that in the v2.0 release the package KOW_View.Service_Mappings was extinguished


		Response	: AWS.Response.Data;
		Component	: Component_Access := Registry.Get_Component( Request );

		function Request_Mode return Request_Mode_Type is
			Params	: AWS.Parameters.List := AWS.Status.Parameters( Request );
		begin
			return Request_Mode_Type'Value( AWS.Parameters.Get( Params, "mode" ) );
		exception
			when others => return Custom_Request;
		end Request_Mode;


		My_Action : KOW_Sec.Accounting.Base_Action_Type'Class := KOW_Sec.Accounting.New_Action(
										Name		=> "request:" & AWS.Status.URI( Request ),
										Root_Accountant	=> Accountant'Access
									);
	begin
		case Request_Mode is
			when Json_Request =>
				declare
					Object : KOW_Lib.Json.Object_Type;
				begin
					Process_JSon_Request(
							Component	=> Component.all,
							Request		=> Request,
							Response	=> Object 
						);

					Response := KOW_View.Json_Util.Build_Success_Response( Object );
					KOW_Sec.Accounting.Set_Exit_Status(
							My_Action,
							KOW_Sec.Accounting.Exit_Success,
							"finished json request"
						);
	

				exception
					when e : REDIRECT =>
						Response := KOW_View.Json_Util.Build_Redirect_Response( Ada.Exceptions.Exception_Message( e ) );
					--when e : REDIRECT_TO_HOME | KOW_Sec.LOGIN_REQUIRED =>
					-- NOTICE:: json responses should be treated as .. well... JSON!
					-- so we don't reraise any occurence here.. simply return it to JS to handle
					when e : others =>
						Response := KOW_View.Json_Util.Build_Error_Response( E );
						KOW_Sec.Accounting.Set_Exit_Status(
								My_Action,
								KOW_Sec.Accounting.Exit_Error,
								"json_error:" & Ada.Exceptions.Exception_Name( e )
							);


				end;

			when Custom_Request =>
				Process_Custom_Request(
						Component	=> Component.all,
						Request		=> Request,
						Response	=> Response
					);
				KOW_Sec.Accounting.Set_Exit_Status(
						My_Action,
						KOW_Sec.Accounting.Exit_Success,
						"finished custom request"
					);
		end case;


		return Response;
	exception

		when e : REDIRECT =>
			KOW_Sec.Accounting.Set_Exit_Status(
					My_Action,
					KOW_Sec.Accounting.Exit_Warning,
					"redirecting to " & Ada.Exceptions.Exception_Message( e )
				);
			return AWS.Response.URL( Ada.Exceptions.Exception_Message( e ) );

		when REDIRECT_TO_HOME =>
			KOW_Sec.Accounting.Set_Exit_Status(
					My_Action,
					KOW_Sec.Accounting.Exit_Warning,
					"redirected to home"
				);

			return AWS.Response.URL( To_String( Home ) );


		when KOW_Sec.LOGIN_REQUIRED =>
			KOW_Sec.Accounting.Set_Exit_Status(
					My_Action,
					KOW_Sec.Accounting.Exit_Warning,
					"redirected to login page"
				);

			return AWS.Response.URL( To_String( Login_Page ) );


		when e : others =>
			KOW_Sec.Accounting.Set_Exit_Status(
					My_Action,
					KOW_Sec.Accounting.Exit_Fatal,
					"Fatal error reraised to AWS.Server! " & Ada.Exceptions.Exception_Name( e )
				);


			Ada.Exceptions.Reraise_Occurrence( e );
	end Process_Request;




	Exception_Template_Root_Path : constant String := "data" / "exceptions";

	procedure Handle_Exception(
				E	: in Ada.Exceptions.Exception_Occurrence;
				Log	: in out AWS.Log.Object;
				Error	: in     AWS.Exceptions.Data;
				Answer	: in out AWS.Response.Data
			) is
		-- Given that the exeption page will be stored in:
		-- 	./data/exceptions/package.otherpackage.exception	=> page for the given exception
		-- 	./data/exceptions/others.html				=> fallback
		--
		use Templates_Parser;
		use Ada.Exceptions;
		Parameters	: Translate_Set;


		function lower_name return String is
		begin
			return Ada.Characters.Handling.To_Lower( KOW_Lib.String_Util.Str_Replace( From => '.', To => '_', Str => Exception_Name( E ) ) );
		end lower_name;

		Specific_Path	: constant String := Exception_Template_Root_Path / lower_name & ".html";
		Others_Path	: constant String := Exception_Template_Root_Path / "others.html";


	begin
		Insert( Parameters, Assoc( "exception_name", Exception_Name( E ) ) );
		Insert( Parameters, Assoc( "exception_message", Exception_Message( E ) ) );
		Insert( Parameters, Assoc( "exception_information", Exception_Information( E ) ) );
		-- TODO :: find out how to get the stack trace when available
		-- TODO :: find out what other information might be important here


		if Ada.Directories.Exists( Specific_Path ) then
			Answer := AWS.Response.Build(
						Content_Type	=> "text/html",
						Message_Body	=> Templates_Parser.Parse( Specific_Path, Parameters )
					);
		elsif Ada.Directories.Exists( Others_Path ) then
			Answer := AWS.Response.Build(
						Content_Type	=> "text/html",
						Message_Body	=> Templates_Parser.Parse( Others_Path, Parameters )
					);
			if E_Mail_On_Exceptions then
				declare
					function T( U : in Unbounded_String ) return String renames To_String;
					My_Action : KOW_Sec.Accounting.Base_Action_Type'Class := KOW_Sec.Accounting.New_Action(
											Name		=> "exception email",
											Root_Accountant	=> Accountant'Access
										);


					Server		: AWS.SMTP.Receiver := AWS.SMTP.Initialize( T( E_Mail_SMTP_Server ) );
					Attachments	: AWS.SMTP.Client.Attachment_Set( 2 .. 1 );
					Status		: AWS.SMTP.Status;
				begin
					AWS.SMTP.Client.Send(
							Server		=> Server,
							From		=> AWS.SMTP.E_Mail( T( E_Mail_From_Name ), T( E_Mail_From_Address ) ),
							To		=> AWS.SMTP.E_Mail( T( E_Mail_To_Name ), T( E_Mail_To_Address ) ),
							Subject		=> T( E_Mail_Subject ) & Exception_Name( E ),
							Message		=> Exception_Information( E ), -- TODO :: maybe there is a better exception message I can send
							Attachments	=> Attachments,
							Status		=> Status
						);
					if AWS.SMTP.Is_OK( Status ) then
						KOW_Sec.Accounting.Set_Exit_Status(
								My_Action,
								KOW_Sec.Accounting.Exit_Success,
								"e-mail sent"
							);
					else
						KOW_Sec.Accounting.Set_Exit_Status(
								My_Action,
								KOW_Sec.Accounting.Exit_Error,
								"e-mail not sent"
							);
					end if;
				end;
			end if;

		else
			Ada.Text_IO.Put_Line( "BIG WARNING :: there is no default exception page template!" );
			Answer := AWS.Response.Build(
						Content_Type	=> "text/html",
						Message_Body	=> "<html><head><title>Oops...</title><body>the server isn't exactly well configured...</body></html>"
					);
		end if;
	end Handle_Exception;

end KOW_View;
