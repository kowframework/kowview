package body My_Page is
	
	function New_Page(Pg_Name : Unbounded_String; Pg_Adb_Name : Unbounded_String) return Page is
		pg : Page;	
	begin		
		pg.Pg_Name := Pg_Name;
		pg.Pg_Adb_Name := Pg_Adb_Name;
		pg.Map_Of_Config_View := Get_Config_File_As_Map(To_String(Pg_Adb_Name));
		
		pg.Css_Name_Files_Vector :=  Aw_Lib.String_Util.explode( SEPARADOR,  Element ( pg.Map_Of_Config_View, To_Unbounded_String( CSS_KEY ) ) );
		pg.Js_Name_Files_Vector :=  Aw_Lib.String_Util.explode( SEPARADOR,  Element ( pg.Map_Of_Config_View, To_Unbounded_String( JS_KEY ) ) );
		Img_Name_Files_Vector	:=  Aw_Lib.String_Util.explode( SEPARADOR,  Element ( pg.Map_Of_Config_View, To_Unbounded_String( IMG_KEY ) ) );
		Mod_Name_Files_Vector   :=  Aw_Lib.String_Util.explode( SEPARADOR,  Element ( pg.Map_Of_Config_View, To_Unbounded_String( MOD_KEY ) ) );

		Header_File_Name := Element ( pg.Map_Of_Config_View, To_Unbounded_String( HEADER_KEY );
		Footer_File_Name := Element ( pg.Map_Of_Config_View, To_Unbounded_String( FOOTER_KEY );
	
		return pg;
	end  New_Page;
	

	function Get_Config_File_As_Map( FileName : String ) return Aw_Lib.UString_Ordered_Maps.Map is
		--carrega arquivo
		Cfg_File: Aw_config.Config_File;
	begin	
		--TODO arrumar o path.
		Aw_config.Add_Config_Path( "/home/vampeta/Desktop/aws/src" );
		
		Cfg_File := Aw_config.New_Config_File( Filename, new Aw_Config.xml.Parser);
		
		
		return Aw_config.Get_Contents_map( Cfg_File );
	end Get_Config_File_As_Map;
	
end My_Page;
