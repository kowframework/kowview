with Aw_Lib.UString_Ordered_Maps; use Aw_Lib.UString_Ordered_Maps;

with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with Aw_config;

with Aw_config.xml;

with my_page;				use my_page;

package Server_Utils is
	
	function Load_Config_File(FileName : String) return Aw_Lib.UString_Ordered_Maps.Map;

	procedure Pages_Factory(Pages_Map: Aw_Lib.UString_Ordered_Maps.Map; Page_Name : Unbounded_String);
	
end Server_Utils;
