



with KOW_View.Driver;





with Ada.Command_line;		use Ada.Command_line;
with Ada.Exceptions;
with Ada.Text_IO;




procedure kvdriver is
begin
	if Argument_Count = 0 then
		raise KOW_View.Driver.USAGE_ERROR with "no command given";
	end if;



	if Argument( 1 ) = "help" then
		KOW_View.Driver.Print_usage;
	else
		raise KOW_View.Driver.USAGE_ERROR with "unknown command """ & Argument( 1 ) & """";
	end if;
exception
	when e : KOW_View.Driver.USAGE_ERROR =>
			Ada.Text_IO.Put_Line( "Usage error: " & Ada.Exceptions.Exception_Message( e ));
			Ada.Text_IO.New_Line;
			KOW_View.Driver.Print_Usage;
end kvdriver;
