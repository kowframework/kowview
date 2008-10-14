with AWS.Response;
with AWS.Server;
with AWS.Status;
with Ada.Text_IO;
with templates_parser;
with Gnat.Io; use Gnat.Io;
WITH Ada.Integer_Text_IO;
WITH Ada.Characters.Handling;
WITH Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


procedure webserver is
	--------------------------declarações para os templates -------------------------------------
	use type Templates_Parser.Vector_Tag;
	QtdLetras   : Templates_Parser.Tag;        
	Letras      : Templates_Parser.Tag;        
	Translations : constant Templates_Parser.Translate_Table:= (1 => Templates_Parser.Assoc ("QTD", QtdLetras),2 => Templates_Parser.Assoc ("LETRA", Letras));

	--------------------------declarações para os contagem de letras -------------------------------------
	objArquivo  : Ada.Text_IO.File_Type;
	letra       : Character;

	--variavel que so cabe letra maiuscula
	SUBTYPE UpperCase IS Character RANGE 'A'..'Z';

	--variavel que so cabe letra minusculas
	SUBTYPE LowerCase IS Character RANGE 'a'..'z';

	--definição de um tipo de array como tipo  "UpperCase" que é conjunto das letras maiusculas
	TYPE LetterCountArray IS ARRAY (UpperCase) OF Natural;
	TYPE LetterFlags      IS ARRAY (UpperCase) OF Boolean;

	--array de  contador de letras 
	LetterCount : LetterCountArray;
	--array de letras 
	LetterFound : LetterFlags;

	--------------------------declaração e função do web server  -------------------------------
	WS : AWS.Server.HTTP;
	function Service (Request : in AWS.Status.Data ) return AWS.Response.Data is
	begin
		LetterCount := (OTHERS => 0);             -- zera todos os itens do array contador  de letras
		LetterFound := (OTHERS => False);         -- zera todos os itens do array com as letras



		--abre o arquivo 
		Ada.Text_IO.Open (File=>objArquivo, Mode=>Ada.Text_IO.In_File, Name=>"teste.txt");
	
		--faz até acabar o arquivo
		LOOP
		EXIT WHEN Ada.Text_IO.End_of_File(File => ObjArquivo);   
			Ada.Text_IO.Get(File => ObjArquivo, Item => letra);
			IF letra IN UpperCase THEN
				LetterCount(letra) := LetterCount(letra) + 1;
				LetterFound(letra) := True;
			ELSIF letra IN LowerCase THEN
				letra := Ada.Characters.Handling.To_Upper(letra);
				LetterCount(letra) := LetterCount(letra) + 1;
				LetterFound(letra) := True;
			END IF;
		END LOOP;
		--fecha arquivo		
		Ada.Text_IO.Close(File => ObjArquivo);

		--fazer com todas as letras do alfabeto
		FOR WhichChar IN UpperCase LOOP
			--se esta true no array de letaras na posição winchar
			IF LetterFound(WhichChar) THEN
				Letras := Letras  & WhichChar;
				QtdLetras   := QtdLetras & LetterCount(WhichChar)  ;
			END IF;
		END LOOP;

	--LIMPA AS VARIAVEIS JPA USADADS PARA IMPEDIR RESULTADOS DUPLICADOS
	Templates_Parser.Clear(QtdLetras);
	Templates_Parser.Clear(Letras);

	--joga no navegador
	return AWS.Response.Build ("text/html", To_String( Templates_Parser.Parse ("table.tmplt", Translations)));

	end Service;

--inicio da funcao webserver
begin

	AWS.Server.Start (WS, "QQ Titulo", Service'Unrestricted_Access);
	AWS.Server.Wait  (AWS.Server.Q_Key_Pressed);

end webserver;
