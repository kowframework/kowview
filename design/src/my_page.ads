with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with Aw_Lib.UString_Ordered_Maps; 	use Aw_Lib.UString_Ordered_Maps;

with Aw_config;

with Aw_config.xml;

with Aw_Lib.UString_Vectors;		use Aw_Lib.UString_Vectors; 

with Aw_Lib.String_Util;


package My_Page is
	
	SEPARADOR : constant Character := ';';
	HEADER_KEY : constant String  := "files.header";
	CSS_KEY : constant String  := "files.css";
	JS_KEY : constant String  := "files.js";
	IMG_KEY : constant String  := "files.img";
	FOOTER_KEY : constant String  := "files.footer";
	MODS_KEY : constant String  := "files.mods";
	POSITIONS_KEY : constant String  := ".positions";


	type Page is record
		Pg_Name : Unbounded_String;
		Pg_Adb_Name : Unbounded_String;
		Map_Of_Config_View : Aw_Lib.UString_Ordered_Maps.Map;

		Css_Name_Files_Vector: Aw_Lib.UString_Vectors.Vector;
		Js_Name_Files_Vector: Aw_Lib.UString_Vectors.Vector;
		Img_Name_Files_Vector: Aw_Lib.UString_Vectors.Vector;
		Mod_Name_Files_Vector: Aw_Lib.UString_Vectors.Vector;
		Header_File_Name : Unbounded_String;
		Footer_File_Name : Unbounded_String;
		
	end record;

	function New_Page(Pg_Name : Unbounded_String; Pg_Adb_Name : Unbounded_String) return Page;

	function Get_Config_File_As_Map(FileName : String) return Aw_Lib.UString_Ordered_Maps.Map;
 
end My_Page;
