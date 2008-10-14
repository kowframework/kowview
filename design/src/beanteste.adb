package body BeanTeste is

	NOT_POSITIONS_DEF : Exception;
	
	MODS_CONFIG_NOT_FOUND : Exception;


	--TODO add coments
	function Make_Bean( Request : in AWS.Status.Data ) return AWS.Response.Data is
		use Ada.Strings.Unbounded;

		--pega nome do arquivo	
		URI		: constant String := AWS.Status.URI( Request );
		Filename	: constant String := URI( URI'First + 1 .. URI'Last );

	
		Translations : Templates_Parser.Translate_Set;
		parse_map: Aw_Lib.UString_Ordered_Maps.Map;
		
		isHaveHeader: boolean := false;

	begin	
		
		begin
			parse_map := Get_Map( Filename );
		exception
			when e: AW_CONFIG.FILE_NOT_FOUND =>
				return AWS.Response.Build (
					Content_Type	=> "text/html",
					Message_Body	=> ( "arquivo de xml nao encontrado: " & Filename)
					);
		end;
		
		if (parse_map.Is_Empty) then
			return AWS.Response.Build (
				Content_Type	=> "text/html",
				Message_Body	=> "Its impossible to create de page. The XML arquive is empty, babformated or not exist."
				);	
		end if;

		
		--caso tenha coisa no mapa, continua lendo o mesmo.
		
		--debug
			--put_map(parse_map);
		--debug		
		
		--carrega o header		
		Load_Header( parse_map, Translations);
		Load_CSS( parse_map, Translations);
		Load_Mods(parse_map, Translations);
		Load_Footer( parse_map, Translations);
		
		
		return AWS.Response.Build (
			Content_Type	=> "text/html",
			Message_Body	=> Templates_Parser.Parse ("teste.tmplt", Translations)
			);
	end Make_Bean;



	--TODO add coments
	function Get_Map( FileName : String ) return Aw_Lib.UString_Ordered_Maps.Map is
		--carrega arquivo
		Cfg_File: Aw_config.Config_File;
	begin	
		
		Aw_config.Add_Config_Path( "/home/vampeta/Desktop/aws/src" );
		
		Cfg_File := Aw_config.New_Config_File( Filename, new Aw_Config.xml.Parser);
		
		return Aw_config.Get_Contents_map( Cfg_File );
	end Get_Map;


	--TODO add coments
	procedure Load_Header(map : in out Aw_Lib.UString_Ordered_Maps.Map; Translations : in out Templates_Parser.Translate_Set) is 
		use Aw_lib.UString_Ordered_Maps;
		use Ada.Strings.Unbounded;

		isHaveHeader: boolean := false;
		header_name: Unbounded_String; 
	begin	
		if Contains( map, To_Unbounded_String( header_key ) ) then
			isHaveHeader := true;			
			Templates_Parser.Insert(Translations, Templates_Parser.Assoc ("isHaveHeader", isHaveHeader) );
			
			header_name := Element (map, To_Unbounded_String( header_key ) );
			Templates_Parser.Insert(Translations, Templates_Parser.Assoc ("header", header_name) );
		end if;
	end Load_Header;


	--TODO add coments
	procedure Load_CSS(map : in out Aw_Lib.UString_Ordered_Maps.Map; Translations : in out Templates_Parser.Translate_Set) is 
		use Aw_lib.UString_Ordered_Maps;
		use Ada.Strings.Unbounded;

		isHaveCSS: boolean := false;
		css_name: Unbounded_String; 
	begin	
		if Contains( map, To_Unbounded_String( css_key ) ) then
			isHaveCSS := true;			
			Templates_Parser.Insert(Translations, Templates_Parser.Assoc ("isHaveCSS", isHaveCSS) );			

			css_name := Element (map, To_Unbounded_String( css_key ) );
			Templates_Parser.Insert(Translations, Templates_Parser.Assoc ("css", css_name) );			
		end if;
	end Load_CSS;



	--TODO add coments
	procedure Load_Footer(map : in out Aw_Lib.UString_Ordered_Maps.Map; Translations : in out Templates_Parser.Translate_Set) is 
		use Aw_lib.UString_Ordered_Maps;
		use Ada.Strings.Unbounded;

		isHaveFooter: boolean := false;
		footer_name: Unbounded_String; 
	begin	
		if Contains( map, To_Unbounded_String( footer_key ) ) then
			isHaveFooter := true;			
			Templates_Parser.Insert(Translations, Templates_Parser.Assoc ("isHaveFooter", isHaveFooter) );			

			footer_name := Element (map, To_Unbounded_String( footer_key ) );
			Templates_Parser.Insert(Translations, Templates_Parser.Assoc ("footer", footer_name) );		
		end if;
	end Load_Footer;		


	--TODO add coments.
	procedure put_map( map : Aw_Lib.UString_Ordered_Maps.Map) is
		use Aw_lib.UString_Ordered_Maps;
		use Ada.Strings.Unbounded;		
		
		--debug
		b : boolean := Contains( map, To_Unbounded_String("header") );
		cu : Aw_lib.UString_Ordered_Maps.Cursor := First(map);
	begin
		Ada.Text_IO.Put_Line( "*****");		
		while Has_Element(cu) loop
			Ada.Text_IO.Put_Line( To_String(Key(cu)) );
			Next(cu);
		end loop;
		Ada.Text_IO.Put_Line( "*****");	
	end put_map;



	--TODO add coments
	procedure Load_Mods(map : in out Aw_Lib.UString_Ordered_Maps.Map; Translations : in out Templates_Parser.Translate_Set) is 
		use Aw_lib.UString_Ordered_Maps;
		use Ada.Strings.Unbounded;

		parse_map_mods : Aw_Lib.UString_Ordered_Maps.Map;
		
		isHaveMods: boolean := false;
		name_file_config: Unbounded_String; 
		Filename : Unbounded_String;
	begin	
		if Contains( map, To_Unbounded_String( mods_key ) ) then
			if not Contains( map, To_Unbounded_String( positions_key ) ) then
				raise NOT_POSITIONS_DEF with "Não foram definidas as posicoes dos modulos em seu arquivo de templete";
			end if;
			
			Filename := Element (map, To_Unbounded_String( mods_key ) );
			begin
				parse_map_mods := Get_Map( TO_String( Filename ) );
			exception
			when e: AW_CONFIG.FILE_NOT_FOUND =>
				raise MODS_CONFIG_NOT_FOUND with "O arquivo de configuracao mods.cfg.xml nao foi encontrado.";
			end;
			
			Mods_Processor(map, parse_map_mods, Translations);
			--isHaveFooter := true;			
			--TODO alocar			
			--Translations(7) := Templates_Parser.Assoc ("isHaveFooter", isHaveFooter);
			--footer_name := Element (map, To_Unbounded_String( footer_key ) );
			--Translations(8) := Templates_Parser.Assoc ("footer", footer_name);			
		end if;
	end Load_Mods;		

	--TODO add coments.
	procedure Mods_Processor(
				Map		: in out Aw_Lib.UString_Ordered_Maps.Map;
				Parse_Map_Mods	: in out Aw_Lib.UString_Ordered_Maps.Map;
				Translations	: in out Templates_Parser.Translate_Set
			) is

		-- This processor produces modules instances and se tehe Translations set so our callback can
		-- printout the resulting page

		use Aw_lib.UString_Ordered_Maps;
		use Ada.Strings.Unbounded;		
		use Aw_Lib.String_Util;
		use type Templates_Parser.Vector_Tag;

		Config_Cursor		: Aw_lib.UString_Ordered_Maps.Cursor;
		Modules_Config_Cursor	: Aw_lib.UString_Ordered_Maps.Cursor;


		key_map : Unbounded_String;
		key_mods : Unbounded_String;	
		
		elemento : Unbounded_String;
		separetor : constant Character := ';';
		vector_keys: Aw_Lib.UString_Vectors.Vector;
		


		Vector_Mod_Parametres : Templates_Parser.Vector_Tag; 


		procedure Positions_Iterator( C: Aw_Lib.UString_Vectors.Cursor ) is
			-- iterate over all positions declared in my configuration file
			-- this is the main loop we have in this function!

			Current_Position_Id : Unbounded_String;
			i : Integer := 1;

			procedure Set_Current_Position_Id( i: in Integer ) is
			begin
				Current_Position_Id := Current_Position_Id & "." & Trim(To_Unbounded_String( Integer'Image(i) ), Ada.Strings.BOTH);
			end Set_Current_Position_Id;

		begin
			Current_Position_Id := Aw_Lib.UString_Vectors.Element( C );
			
			begin
				Modules_Config_Cursor := Aw_lib.UString_Ordered_Maps.Find(parse_map_mods, Current_Position_Id );
			exception
			when e: CONSTRAINT_ERROR =>
				raise CONSTRAINT_ERROR with "Deu erro na chave: " & To_String(Current_Position_Id);
			end;
			
			--debug			
				--Ada.Text_IO.Put_Line( "*****");
				--Ada.Text_IO.Put_Line( "Rodando loop chave: " & To_String(Aw_Lib.UString_Vectors.Element(C)));
				--Ada.Text_IO.Put_Line( "Usando chave: " & To_String(Current_Position_Id));		
				--Ada.Text_IO.Put_Line( "*****");		
			--end debug

			begin

				while Has_Element(Modules_Config_Cursor) loop
					elemento := Element(parse_map_mods, Current_Position_Id);
						
					--debug	
						--Ada.Text_IO.Put_Line( "");				
						--Ada.Text_IO.Put_Line( "*****");
						--Ada.Text_IO.Put_Line( "Rodando inner-loop chave: " & To_String(elemento));	
					--debug
					Vector_Mod_Parametres := + Vector_Mod_Parametres & To_String(elemento);		
						
					Aw_lib.UString_Ordered_Maps.Next(Modules_Config_Cursor);
					i := i+1;
					Current_Position_Id := Aw_Lib.UString_Vectors.Element(C);
					Current_Position_Id := Current_Position_Id & "." & Trim(To_Unbounded_String( Integer'Image(i) ), Ada.Strings.BOTH);
				
					--debug				
						--Ada.Text_IO.Put_Line( "Gerando nova chave: " & To_String(Current_Position_Id));	
						--Ada.Text_IO.Put_Line( "*****");	
						--Ada.Text_IO.Put_Line( "");	
					--debug	
				end loop;
			exception
				when e: CONSTRAINT_ERROR =>
					null;
			end;
			--associa i vetor de parametros ao nome do modulo.
			Templates_Parser.Insert(Translations, Templates_Parser.Assoc(To_String(Aw_Lib.UString_Vectors.Element(C)), Vector_Mod_Parametres) );
			--reinicializa contador
			i := 1;
		end Positions_Iterator;

	begin
				
		vector_keys :=  Aw_Lib.String_Util.explode( separetor,  Element ( To_Unbounded_String( positions_key ) ) );
		
		if Aw_Lib.UString_Vectors.Is_Empty(vector_keys)then
			raise MODS_CONFIG_NOT_FOUND with "O arquivo de configuracao mods.cfg.xml nao esta bem definido.";		
		end if;

		Aw_Lib.UString_Vectors.Iterate( Positions_Vector, Positions_Iterator'Access );
		-- initialize the map of Unbounded_Strings for each module.

		Aw_Lib.UString_Maps.Iterator( Modules_Map, Modules_Iterator'Access );
		-- append the module results for each module for the corresponding region

	end Mods_Processor;







	procedure Register_Module_Processor( Name: in Unbounded_String; Processor: in Module_Processor_Type ) is
	-- Register a module processor to be used in modules called "Name"
		
	begin
		null;
	end Register_Module_Processor;
	

	procedure Register_Module_Processor( Name: in String; Processor: in Module_Processor_Type ) is
	-- Register a module processor to be used in modules called "Name"
	begin 
		null;
	end Register_Module_Processor;

	procedure Default_Module_Factory(
				Module_Name		: in Unbounded_String;
				Global_Translations	: in Templates_Parser.Translate_Set;
				Output_String		: in out Unbounded_String
			) is
	begin
		null;
	end Default_Module_Factory;
	
end BeanTeste;
