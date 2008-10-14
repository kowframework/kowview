

package Contador is



	--------------------------declarações para os contagem de letras -------------------------------------


	subtype Known_Range is Character range 'a'..'z';
	-- variavel que so cabe letra minusculas
	-- serei case insensitive, interpretando até as maiúsculas como minúsculas

	-- definição de um tipo de array como tipo  "UpperCase" que é conjunto das letras maiusculas


	type Letter_Count_Array is Array( Known_Range range ) of Natural;


	function Conta_Texto( File_Name: in String ) return Letter_Count_Array;
	-- conta os caracteres e retorna um array com seus valores


end Contador;
