
with Ada.Text_IO;





-- we load the commands here ..
with KOW_View.Help;
pragma Elaborate_Body( KOW_View.Help );

package body KOW_View.Driver is


	procedure Run_Command( Command : in Available_Commands ) is
	begin
		KOW_View.Commands.Run( Get( Command ) );
	end Run_Command;


	procedure Register( Command : in Available_Commands; Constructor : function return KOW_View.Driver_Commands.Command_Type'Class ) is
	begin
		pragma Assert( Command_Constructors( Command ) = null );

		Command_Constructors( Command ) := Constructor;
	end Register;



	function Get( Command : in Available_Commands ) return KOW_View.Driver_Commands.Command_type'Class is
	begin
		if Command_Constructors( Command ) = NULL then
			raise PROGRAM_ERROR with "command is not registered..";
		end if;

		return Command_Constructors( Command ).all;
	end Get;


end KOW_View.Driver;
