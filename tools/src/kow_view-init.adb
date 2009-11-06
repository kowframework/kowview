
with Ada.Command_Line;
with Ada.Text_IO;




with KOW_View.Commands;


with KOW_View.Driver;

----------------------------------------
-- Implementation of the Init command --
----------------------------------------

package body KOW_View.Init is


	function New_Command return KOW_View.Commands.Command_Type'Class is
		Command : Command_Type;
	begin
		
		return Command;
	end New_Command;



	overriding
	procedure Run( Command : in out Command_Type ) is
		-- initialize the project
		-- required parameter :: project name
	begin
		if Ada.Command_Line.Argument_Count /= 2 then
			raise KOW_View.Commands.USAGE_ERROR with "the init command requires a parameter with the project name";
		end if;
	end Run;


	overriding
	procedure Help( Command : in out Command_Type ) is
		-- show detailed information about this command
		use Ada.Command_Line;
		use Ada.Text_IO;
	begin
		Put_Line( "Usage:" );
		Put_Line( Command_Name & " init project_name" );
		New_Line;
		Put_Line( "Initialize the project folder, where ""project_name"" is a valid compilation unit in Ada for your project" );
	end Help;


	overriding
	procedure Describe( Command : in out Command_Type ) is
		-- show a short description about this command;
	begin
		Ada.Text_IO.Put( "initialize the project folder" );
	end Describe;
end KOW_View.Init;
