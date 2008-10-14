with Ada.Text_IO;

-- The Server
with AWS.Server;


-- For the callback..
with AWS.Status;
with AWS.Response;

-- Helper packages..
with AWS.MIME;
with AWS.Utils;


--with beanteste;
--use beanteste;

with Aw_Lib.UString_Ordered_Maps;

with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with server_utils;	use server_utils;


procedure Server is
	Web_Server : AWS.Server.HTTP;
	
	Faces_Config_File_Name : constant String := "faces";

	count: Positive := 1;
	
	--mapa de paginas da aplicacao.
	Pages_Map : Aw_Lib.UString_Ordered_Maps.Map;


	function My_Callback( Request : in AWS.Status.Data ) return AWS.Response.Data is
		URI		: constant String := AWS.Status.URI( Request );
		Filename	: constant String := URI( URI'First + 1 .. URI'Last );
		

	begin	

		if Aw_Lib.UString_Ordered_Maps.Contains( Pages_Map, To_Unbounded_String(Filename) ) then
			--pega o nome do arquivo ads e instancia o mesmo.
			Pages_Factory( Pages_Map, To_Unbounded_String(Filename) );

			
			return AWS.Response.File(
				Content_Type	=> AWS.MIME.Content_Type (Filename),
				Filename	=> Filename);
		--elsif URI = "/beanteste" then
		--	return Make_Bean( Request );
		else
			count := count + 1;
			return AWS.Response.Build (
					Content_Type	=> "text/html",
					Message_Body	=> "not found: " & Positive'Image( count ) & " @ " & URI
				);
		end if;

	end My_Callback;
begin

	AWS.Server.Start(
		Web_Server			=> Web_Server,
		Name				=>"My Web Server",
		Callback			=> My_Callback'Unrestricted_Access,
		Max_Connection			=> 10,
		Admin_URI			=> "/admin",
		Port 				=> 3030,
		Security 			=> false, -- tem ssl?
		Session 			=> true,
		Case_Sensitive_Parameters	=> True,
		Upload_Directory	 	=> "/tmp");

	Ada.Text_IO.Put_Line( " Subiu o servidor!!" );	
	

	--carrega o mapa de paginas da aplicacao.
	Pages_Map := Load_Config_File(Faces_Config_File_Name);


	for Index in 1..31 loop
         	delay 1.0;
		Ada.Text_IO.Put_Line( "Contagem regressiva: " );	
      	end loop;

	Ada.Text_IO.Put_Line( " Morreu o servidor!!" );

end Server;
