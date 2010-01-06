

with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Text_IO;		use Ada.Text_IO;


with KOW_View_Tools.Commands;


with KOW_View_Tools.Driver;


----------------------------------------
-- Implementation of the Help command --
----------------------------------------

package body KOW_View_Tools.Help is

	function New_Command return KOW_View_Tools.Commands.Command_Type'Class is
		-- constructor for our command
		Command : Command_Type;
	begin
		return Command;
	end New_Command;


	overriding
	procedure Run( Command : in out Command_Type ) is
		-- when no parameter is given, display the help message


		General_Usage	: Boolean := True;
		Sub_Command	: KOW_View_Tools.Driver.Available_Commands;
	begin

		if Argument_Count = 2 then
			begin
				Sub_Command := KOW_View_Tools.Driver.Available_Commands'Value(
							Argument( 2 )
						);
				General_Usage := False;
			exception
				when CONSTRAINT_ERROR => null;
			end;
		end if;

		if General_Usage then
			Put_Line( "Usage:" );
			Put_Line( Command_Name & " command [command parameters]" );
			Put_Line( "where command can be one of the:" );
	
			for Comm in KOW_View_Tools.Driver.Available_Commands loop
				declare
					use KOW_View_Tools.Driver;
					C : KOW_View_Tools.Commands.Command_Type'Class := Get( Comm );
				begin
					Put( "    "  &  Available_Commands'Image( Comm ) & " => " );
					KOW_View_Tools.Commands.Describe( C );
					New_Line;
				end;
			end loop;
		else
			declare
				Comm : KOW_View_Tools.Commands.Command_Type'class := KOW_View_Tools.Driver.Get( Sub_Command );
			begin
				KOW_View_Tools.Commands.Help( Comm );
			end;
		end if;
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
		Put( "show usage instructions" );
	end Describe;

end KOW_View_Tools.Help;
