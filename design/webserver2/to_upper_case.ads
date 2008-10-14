with Ada.Text_IO;	use Ada.Text_IO;

package to_upper_case is

	Factor: constant Integer   := Character'Pos( 'a' ) - Character'Pos( 'A' );
	function To_Up( C: in Character ) return Character;
	procedure convert	( C: in Character );

end to_upper_case;
