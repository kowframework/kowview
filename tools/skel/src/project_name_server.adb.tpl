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
	@_project_name_@_Setup;
	-- the setup process is where the entire KOW Framework is initialized. :)
	


	----------------------------
	-- Put your own code here --
	----------------------------


	-----------------
	-- AWS Startup --
	-----------------
	AWS.Server.Start(
		Web_Server,
		KOW_View.Service_Mapping.AWS_Callback'Unrestricted_Access,
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
