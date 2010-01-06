
with Ada.Text_IO;





package body KOW_View_Tools.Driver is


	procedure Run_Command( Command : in Available_Commands ) is
		Comm : KOW_View_Tools.Commands.Command_Type'Class := Get( Command );
	begin
		KOW_View_Tools.Commands.Run( Comm );
	end Run_Command;


	function Get( Command : in Available_Commands ) return KOW_View_Tools.Commands.Command_type'Class is
	begin
		if Command_Constructors( Command ) = NULL then
			raise PROGRAM_ERROR with "command is not registered..";
		end if;

		return Command_Constructors( Command ).all;
	end Get;
end KOW_View_Tools.Driver;
