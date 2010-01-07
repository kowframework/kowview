

with KOW_Config;
with KOW_View_Tools.Commands;
with KOW_View_Tools.Driver;





with Ada.Command_line;		use Ada.Command_line;
with Ada.Exceptions;
with Ada.Text_IO;




procedure kvdriver is
	Command : KOW_View_Tools.Driver.Available_Commands;
begin
	KOW_Config.Add_Config_path( "." );

	if Argument_Count = 0 then
		raise KOW_View_Tools.Commands.Usage_ERROR with "no command given";
	end if;


	begin
		Command := KOW_View_Tools.Driver.Available_Commands'Value( Argument( 1 ) );
	exception
		when constraint_error =>
			raise KOW_View_Tools.Commands.Usage_ERROR with "unknown command """ & Argument( 1 ) & """";
	end;


	KOW_View_Tools.Driver.Run_Command( Command );
exception
	when e : KOW_View_Tools.Commands.Usage_ERROR =>
		Ada.COmmand_Line.Set_Exit_Status( Ada.Command_Line.Failure );
		Ada.Text_IO.Put_Line( "Usage error: " & Ada.Exceptions.Exception_Message( e ));
		Ada.Text_IO.New_Line;
		KOW_View_Tools.Driver.Run_Command( KOW_View_Tools.Driver.Help );
end kvdriver;
