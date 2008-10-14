with Ada.Text_IO;	    use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with to_upper_case;


package body contador is



	function AbrirTxt( Nome_Arquivo: in String ) return Contagem_Letras  is 
	MeuArquivo  : Ada.Text_IO.File_Type;

	Contagem_Letras_Array   : Contagem_Letras;
	letra : Character;

	begin
		Contagem_Letras_Array( 'A' ) := 0 ;
				
		Ada.Text_IO.Open (File=>MeuArquivo, Mode=>Ada.Text_IO.In_File, Name=>Nome_Arquivo);
		--abrir arquivo
		LOOP

		EXIT WHEN Ada.Text_IO.End_of_File( File => MeuArquivo );   
			--fazer até o final do arquivo

			put("___");

			Ada.Text_IO.Get( File => MeuArquivo, Item => letra );
			--pegar arquivo e ir jogando em letra  

			to_upper_case.convert( letra );
			
			
--				Contagem_Letras_Array( letra ) := Contagem_Letras_Array( letra ) + 1;
		END LOOP;
		return Contagem_Letras_Array ;
	end AbrirTxt;








end contador ;
