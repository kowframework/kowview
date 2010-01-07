



--------------
-- Ada 2005 --
--------------
with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Text_IO;		use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_View_Tools.Commands;


----------------------------------------
-- Implementation of the Info command --
----------------------------------------

package body KOW_View_Tools.Info is



	function New_Command return KOW_View_Tools.Commands.Command_Type'Class is
		-- constructor for our command
		Command : Command_Type;
	begin
		return Command;
	end New_Command;



	overriding
	procedure Run( Command : in out Command_Type ) is
		-- display some information about the project
	begin
		Put_Line( "Project Name :: " & Project_Name );
	exception
		when constraint_error => raise CONSTRAINT_ERROR with "this is not a kvdriver project.. run init first";
	end Run;



	overriding
	procedure Help( Command : in out Command_Type ) is
		-- show detailed information about this command
	begin
		Put_Line( "Usage:" );
		Put_Line( Command_Name & " info" );
		New_Line;
		Put_Line( "Show some simple information about the current project" );

	end Help;


	overriding
	procedure Describe( Command : in out Command_Type ) is
		-- show a short description about this command;
	begin
		Put( "show some simple information about the current project" );
	end Describe;
end KOW_View_Tools.Info;
