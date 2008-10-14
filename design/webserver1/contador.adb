

package body Contador is



	-- variavel que so cabe letra minusculas
	-- serei case insensitive, interpretando até as maiúsculas como minúsculas

	-- definição de um tipo de array como tipo  "UpperCase" que é conjunto das letras maiusculas

	subtype Letras_Validas is Character range 'A'..'Z';
	type Letter_Count_Array is Array( Letras_Validas range ) of Natural;


	function Conta_Texto( File_Name: in String ) return Letter_Count_Array is
		-- conta os caracteres e retorna um array com seus valores
		-- TODO: IMPLEMENTAR
		--	conta todos os caracteres como maiusculo, ignorando nao conhecidos
	objArquivo  : Ada.Text_IO.File_Type;
	letra       : Character;

	begin
		letra := 'A' - 'a' + 1; -- == 'a'
		
		for CADA_CARACTER NO TEXTO loop
			begin
				-- aqui dentro roda o T.To_Up;
			exception
				when CONSTRAINT_ERROR => null;
				-- ignora quando da o erro CONSTRAINT_ERROR
			end;
	end Conta_Texto;
end Contador;



