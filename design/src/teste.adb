with Ada.Text_IO;

with AWS.Response;
with AWS.Status;

with Templates_Parser;

function Teste( Request : in AWS.Status.Data ) return AWS.Response.Data is
	use type Templates_Parser.Tag;
	use type Templates_Parser.Vector_Tag;
	use type Templates_Parser.Matrix_Tag;
	
	--pergunta ao relatorio quantas colunas tem.	
	tamanhoTabela :integer := 4;
	
	--Iterar o relatorio e obter todos os vetores e nome de colunas e setar em Todos e Name_colunas respectivamente. 	
	Names : constant Templates_Parser.Vector_Tag:= +"Bob" & "Bill" & "Toto" & "Ogro"; 
	SobreNome : constant Templates_Parser.Vector_Tag:= +"Boiola" & "Biba" & "Cachorra" & "Gay"; 
	Apelido : constant Templates_Parser.Vector_Tag:= +"Boilinha" & "Bibinha" & "Cachorrona " & "Chefinho";	
	Ages : constant Templates_Parser.Vector_Tag:= +"10 " & "30 " & "5 " & "24";
	
	--matris de todos os dados e vetor de nome de colunar.
	Todos: constant Templates_Parser.Matrix_Tag := + Names & SobreNome & Apelido & Ages;
	Nome_colunas : constant Templates_Parser.Vector_Tag := + "Nome" & "Sobrenome" & "Apelido" & "Idade";	

	--montar a resposta
	Translations : Templates_Parser.Translate_Table( 1 .. 2 );
begin

	Translations(1) := Templates_Parser.Assoc ("Todos", Todos);
	Translations(2) := Templates_Parser.Assoc ("Names", Nome_colunas);

	
	return AWS.Response.Build (
		Content_Type	=> "text/html",
		Message_Body	=> Templates_Parser.Parse ("../teste.tmplt", Translations)
		);

end Teste;




