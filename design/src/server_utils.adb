package body Server_Utils is
	
	PAGE_NOT_DEFINE : Exception;

	function Load_Config_File( FileName : String ) return Aw_Lib.UString_Ordered_Maps.Map is
		--carrega arquivo
		Cfg_File: Aw_config.Config_File;
	begin	
		
		--TODO arrumar o path.
		Aw_config.Add_Config_Path( "/home/vampeta/Desktop/aws/src" );
		
		Cfg_File := Aw_config.New_Config_File( Filename, new Aw_Config.xml.Parser);
		
		return Aw_config.Get_Contents_map( Cfg_File );
	end Load_Config_File;


	procedure Pages_Factory(Pages_Map: Aw_Lib.UString_Ordered_Maps.Map; Page_Name : Unbounded_String) is

		Adb_File_Name : Unbounded_String;
		Pagina_Atual : Page;

	begin	
		if Contains(Pages_Map, Page_Name) then
			Adb_File_Name := Element(Pages_Map, Page_Name);
			Pagina_Atual := New_Page(Page_Name, Adb_File_Name);
		else
			raise PAGE_NOT_DEFINE with "O arquivo de pagina: " & To_String(Page_Name) & "não existe ou não esta definido.";
		end if;
	
	end Pages_Factory;


end Server_Utils;
