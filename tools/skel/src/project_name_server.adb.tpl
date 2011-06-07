-- This is simply a stub file create automatically by the kvdriver tool.
--
-- Feel free to change things around, but keep in mind you might break
-- things up.
-- 
-- Notice also this file is generated by the kvdriver init command. It's
-- not suposed to change after that.. so.... have fun messing around!




--------------
-- Ada 2005 --
--------------
with Ada.Exceptions;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;		use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_View;


---------
-- AWS --
---------
with AWS.Config;
with AWS.Server;





with @_project_name_@_Setup;


procedure @_project_name_@_Server is
	Web_Server	: AWS.Server.HTTP;
	Conf		: constant AWS.Config.Object := AWS.Config.Get_Current;
begin
	-------------------
	-- Run the Setup --
	-------------------
	@_project_name_@_Setup( true );
	-- the setup process is where the entire KOW Framework is initialized. :)
	

	-----------------------------
	-- Setup the Email Sending --
	-----------------------------

	KOW_View.E_Mail_On_Exceptions	:= False;
	-- send email with information about excetpions that aren't expected
	-- disabled by default on development environments

	KOW_View.E_Mail_From_Name	:= To_Unbounded_String( "KOW Framework Application" );
	KOW_View.E_Mail_From_Address	:= To_Unbounded_String( "app@yourwebsite" );

	KOW_View.E_Mail_To_Name		:= To_Unbounded_String( "Admin" );
	KOW_View.E_Mail_To_Address	:= To_Unbounded_String( "admin@yourwebsite" );
	
	KOW_View.Error_E_Mail_Subject	:= To_Unbounded_String( "[KOW_View Unhandled Exception] " );

	KOW_View.E_Mail_SMTP_Server	:= To_Unbounded_String( "localhost" );



	----------------------------
	-- Put your own code here --
	----------------------------


	------------------------------------------
	-- Set the Unexpected Exception handler --
	------------------------------------------
	AWS.Server.Set_Unexpected_Exception_Handler(
			Web_Server,
			KOW_View.Handle_Exception'Unrestricted_Access
		);
	
	-----------------
	-- AWS Startup --
	-----------------
	AWS.Server.Start(
			Web_Server,
			KOW_View.Process_Request'Unrestricted_Access,
			Conf
		);

	AWS.Server.Wait( AWS.Server.Forever );


exception
	when E : Others =>
		Ada.Text_IO.Put_Line( "=======================================" );
		Ada.Text_IO.Put_Line( "=======================================" );
		Ada.Text_IO.Put_Line( "=======================================" );
		Ada.Text_IO.Put_Line( Ada.Exceptions.Exception_Information( E ) );
		Ada.Text_IO.Put_Line( "=======================================" );
		Ada.Text_IO.Put_Line( "=======================================" );
		Ada.Text_IO.Put_Line( "=======================================" );

end @_project_name_@_Server;
