with Ada.Text_IO;	use Ada.Text_IO;

package contador is

	SUBTYPE Letras_Validas is Character range 'A'..'Z';
	TYPE Contagem_Letras IS ARRAY( Letras_Validas ) OF Natural;

	function AbrirTxt( Nome_Arquivo: in String ) return Contagem_Letras;


end contador;
