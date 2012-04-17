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
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.Json;
with KOW_Lib.Log;
with KOW_Lib.String_Util;
with KOW_Sec;
with KOW_Sec.Accounting;
with KOW_View.Json_Util;
with KOW_View.Request_Dispatchers;
with KOW_View.Security;


---------
-- AWS --
---------
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.SMTP;
with AWS.SMTP.Client;
with AWS.Status;
with Templates_Parser;


package body KOW_View is

	Big_Error_Logger : KOW_Lib.Log.Logger_Type := KOW_Lib.Log.Get_Logger( "KOW_View HUGE Exceptions" );



	function Process_Request( Request : in AWS.Status.Data ) return AWS.Response.Data is
		-- this is the main function... it's the AWS callback used all around.
		-- notice that in the v2.0 release the package KOW_View.Service_Mappings was extinguished


		Response	: AWS.Response.Data;
		Component	: Component_Ptr;

		function Request_Mode return Request_Mode_Type is
			Params	: AWS.Parameters.List := AWS.Status.Parameters( Request );
		begin
			return Request_Mode_Type'Value( AWS.Parameters.Get( Params, "mode" ) & "_request" );
		exception
			when others => return Custom_Request;
		end Request_Mode;


		My_Action : KOW_Sec.Accounting.Base_Action_Type'Class := KOW_Sec.Accounting.New_Action(
										Name		=> "request:" & AWS.Status.URI( Request ),
										Root_Accountant	=> Accountant'Access
									);
		htdocs_path : constant String := "htdocs" & URI;

		Dispatcher : Request_Dispatchers.Request_Dispatcher_Ptr := Request_Dispatchers.Get_Dispatcher( Request );
	begin


		if Dispatcher = null then
			raise ERROR_404 with AWS.Status.URI( Request );
		else
			return Request_Dispatchers.Dispatch( Dispatcher.all, Request );
		end if;

		elsif URI = "/favicon.ico" then
			raise REDIRECT with "/themes/theme/favicon.ico";
		elsif URI = "/robots.txt" then
			raise REDIRECT with "/pages/static/robots.txt";
		end if;

		Component := Component_Ptr( Registry.Get_Component( Request ) );
		case Request_Mode is
			when Json_Request =>
						Response := KOW_View.Json_Util.Build_Error_Response( E => E, Wrap_Data => Wrap_Data );
						KOW_Sec.Accounting.Set_Exit_Status(
								My_Action,
								KOW_Sec.Accounting.Exit_Error,
								"json_error:" & Ada.Exceptions.Exception_Name( e ) & ":" & Ada.Exceptions.Exception_Message( e )
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
					"Fatal error reraised to AWS.Server! " & Ada.Exceptions.Exception_Name( e ) & '(' & Ada.Exceptions.Exception_Message( e ) & ')'
				);


			Ada.Exceptions.Reraise_Occurrence( e );
	end Process_Request;




	Exception_Template_Root_Path : constant String := "data" / "exceptions";

	procedure Handle_Exception(
				E	: in     Ada.Exceptions.Exception_Occurrence;
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




					function Message return String is
						use Templates_Parser;

						Template_Path	: constant String := "./data/exceptions/email.txt";
						User		: KOW_Sec.User_Type;
						P		: Translate_Set;
					begin
						if Ada.Directories.Exists( Template_Path ) then
							User := KOW_View.Security.Get_User( Error.Request );

							KOW_View.Security.Insert( P, User );
							Insert( P, Assoc( "exception_name", Exception_Name( E ) ) );
							Insert( P, Assoc( "exception_message", Exception_Message( E ) ) );
							Insert( P, Assoc( "exception_information", Exception_Information( E ) ) );
							Insert( P, Assoc( "uri", AWS.Status.URI( Error.Request ) ) );
							Insert( P, Assoc( "host", AWS.Status.Host( Error.Request ) ) );
							Insert( P, Assoc( "user_agent", AWS.Status.User_Agent( Error.Request ) ) );
							Insert( P, Assoc( "referer", AWS.Status.Referer( Error.Request ) ) );
							Insert( P, Assoc( "peername", AWS.Status.Peername( Error.Request ) ) );

							Insert( P, Assoc( "parameters", AWS.Parameters.URI_Format( AWS.Status.Parameters( Error.Request ) ) ) );

							return Parse( Template_Path, P );
						else
							return Exception_Information( E );
						end if;
					end Message;
				begin
					AWS.SMTP.Client.Send(
							Server		=> Server,
							From		=> AWS.SMTP.E_Mail( T( E_Mail_From_Name ), T( E_Mail_From_Address ) ),
							To		=> AWS.SMTP.E_Mail( T( E_Mail_To_Name ), T( E_Mail_To_Address ) ),
							Subject		=> T( Error_E_Mail_Subject ) & Exception_Name( E ) & "@" & AWS.Status.Host( Error.Request ),
							Message		=> Message,
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
	exception
		when e : others => 
			KOW_Lib.Log.Log(
					Logger	=> BIG_Error_Logger,
					Level	=> KOW_Lib.Log.Level_Error,
					Message	=> "I AM UNABLE TO HANDLE EXCEPTIONS.... SHAME ON ME"
				);
			KOW_Lib.Log.Log(
					Logger	=> BIG_Error_Logger,
					Level	=> KOW_Lib.Log.Level_Error,
					Message	=> "Name : " & Ada.Exceptions.Exception_Name( E )
				);
			KOW_Lib.Log.Log(
					Logger	=> BIG_Error_Logger,
					Level	=> KOW_Lib.Log.Level_Error,
					Message	=> "Message : " & Ada.Exceptions.Exception_Message( E )
				);



			-- couldn't handle the exception? well, then we stop trying and give up
			Answer := AWS.Response.Build(
						Content_Type	=> "text/html",
						Message_Body	=> "<html><head><title>I give up...</title></head><body>So many erros in my soul... I give up.</body></html>"
					);
	end Handle_Exception;



	------------------------------
	-- Email Sending Procedures --
	------------------------------




	procedure Send_Email(
			To_Name		: in String;
			To_Address	: in String;
			Subject		: in String;
			Message		: in String 
		) is

		function T( U : in Unbounded_String ) return String renames To_String;
		My_Action : KOW_Sec.Accounting.Base_Action_Type'Class := KOW_Sec.Accounting.New_Action(
								Name		=> "email",
								Root_Accountant	=> Accountant'Access
							);

		Server		: AWS.SMTP.Receiver := AWS.SMTP.Initialize( T( E_Mail_SMTP_Server ) );
		Attachments	: AWS.SMTP.Client.Attachment_Set( 2 .. 1 );
		Status		: AWS.SMTP.Status;
	begin
		AWS.SMTP.Client.Send(
				Server		=> Server,
				From		=> AWS.SMTP.E_Mail( T( E_Mail_From_Name ), T( E_Mail_From_Address ) ),
				To		=> AWS.SMTP.E_Mail( To_Name, To_Address ),
				Subject		=> Subject,
				Message		=> Message,
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





	procedure Send_Email(
			To	: in KOW_Sec.User_Data_Type;
			Subject	: in String;
			Message	: in String
		) is
		-- send plain text email messages
		function T( Str : in String ) return String is
		begin
			return Ada.Strings.Fixed.Trim( Str, Ada.Strings.Both );
		end T;

	begin
		Send_Email(
				To_Name		=> T( To.Nickname ),
				To_Address	=> T( To.Primary_Email ),
				Subject		=> Subject,
				Message		=> Message
			);
	end Send_Email;



	function Virtual_Host(
			Request	: in AWS.Status.Data
		) return Virtual_Host_Name_Type is
		-- TODO :: implement-me
		-- compute the current virtual host
	begin
		return Virtual_Host_Name_Type'( others => ' ' );
	end Virtual_Host;
end KOW_View;
