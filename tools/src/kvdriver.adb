



with KOW_View.Driver;





with Ada.Command_line;		use Ada.Command_line;
with Ada.Exceptions;
with Ada.Text_IO;




procedure kvdriver is
	Command : KOW_View.Driver.Available_Commands;
begin

	if Argument_Count = 0 then
		raise KOW_View.Driver.USAGE_ERROR with "no command given";
	end if;


	begin
		Command := KOW_View.Driver.Available_Commands'Value( Argument( 1 ) );
	exception
		when constraint_error =>
			raise KOW_View.Driver.USAGE_ERROR with "unknown command """ & Argument( 1 ) & """";
	end;


	KOW_View.Driver.Run_Command( Command );
exception
	when e : KOW_View.Driver.USAGE_ERROR =>
		Ada.COmmand_Line.Set_Exit_Status( Ada.Command_Line.Failure );
		Ada.Text_IO.Put_Line( "Usage error: " & Ada.Exceptions.Exception_Message( e ));
		Ada.Text_IO.New_Line;
		KOW_View.Driver.Run_Command( KOW_View.Driver.Help );
end kvdriver;
