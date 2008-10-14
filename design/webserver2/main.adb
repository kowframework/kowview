with Ada.Text_IO;	    use Ada.Text_IO;
with Ada.Command_Line;
with to_upper_case;
with contador; 		    use contador;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ada.IO_Exceptions;

procedure main is
bla : Contagem_Letras;
 
begin
	for i in 1 .. Ada.Command_Line.Argument_Count loop
		declare
			My_Arg: String := Ada.Command_Line.Argument( 1 );
		begin
			put(My_Arg);
			New_Line;
			bla := AbrirTxt( My_Arg );
		end;
	end loop;
	exception
	when Ada.IO_Exceptions.Use_Error =>
	Put_Line (""); 
end main;	
