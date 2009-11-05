


with KOW_View.Commands;


-------------------------------------------
-- Implementation of the Version command --
-------------------------------------------

package KOW_View.Version is


	function New_Command return KOW_View.Commands.Command_Type'Class is
		-- constructor for our command
		Command : Command_Type;
	begin
		return Command;
	end New_Command;


	overriding
	procedure Run( Command : in out Command_Type ) is
		-- when no parameter is given, display the version message
	begin
		Put_line( Command_Name & " version 0.1" );
	end Run;


	overriding
	procedure Help( Command : in out Command_Type ) is
		-- show detailed information about this command
	begin
		Describe( Command );
		New_Line;
	end Help;


	overriding
	procedure Describe( Command : in out Command_Type ) is
		-- show a short description about this command;
	begin
		Put( "display the version for the software being used" );
	end Describe;
end KOW_View.Version;
