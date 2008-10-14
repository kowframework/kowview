with Ada.Text_IO;	use Ada.Text_IO;

package body to_upper_case is

	function To_Up( C: in Character ) return Character is
		Pos : Integer   := Character'Pos( C );
	begin
		if C in 'A' .. 'Z' OR C = ' ' then
			return C;
		elsif C in 'a' .. 'z' then
			return Character'Val( Pos - Factor );
		end if;
		raise CONSTRAINT_ERROR with "Não conheço o caracter """ & C & """";
	end To_Up;

	procedure convert( C: in Character ) is
	begin
		Put( To_Up( C ) );
		New_Line;
	end convert;

end to_upper_case;
