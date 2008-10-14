with Ada.Text_IO;

with AWS.Response;
with AWS.Status;

with Templates_Parser;

with Aw_config;

with Aw_config.xml;
use Aw_config.xml;

with Aw_Lib.UString_Ordered_Maps;
with Aw_Lib.UString_Vectors;		use Aw_Lib.UString_Vectors; 


with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

with Ada.IO_Exceptions;

with Aw_Lib.String_Util;


package BeanTeste is


	header_key : constant String  := "files.header";
	css_key : constant String  := "files.css";
	footer_key : constant String  := "files.footer";
	mods_key : constant String  := "files.mods";
	positions_key : constant String  := ".positions";
	-- TODO: fix in Aw_Config.XML parser :: do not put a freaking point when the section is empty!


	function Make_Bean( 
			   Request : in AWS.Status.Data 
		) return AWS.Response.Data;

	
	function Get_Map( 
			FileName : String
		) return Aw_Lib.UString_Ordered_Maps.Map;
	

	procedure Load_Header(
				Map : in out Aw_Lib.UString_Ordered_Maps.Map; 
				Translations : in out Templates_Parser.Translate_Set
		);

	procedure Load_CSS(
				Map : in out Aw_Lib.UString_Ordered_Maps.Map; 
				Translations : in out Templates_Parser.Translate_Set
		);

	procedure Load_Footer(
				Map : in out Aw_Lib.UString_Ordered_Maps.Map; 
				Translations : in out Templates_Parser.Translate_Set
		);	 

	procedure Load_Mods(
				Map : in out Aw_Lib.UString_Ordered_Maps.Map; 
				Translations : in out Templates_Parser.Translate_Set
		);

	procedure put_map( 
				Map : Aw_Lib.UString_Ordered_Maps.Map
		);
	
	procedure Mods_Processor(
				Map		: in out Aw_Lib.UString_Ordered_Maps.Map;
				Parse_Map_Mods	: in out Aw_Lib.UString_Ordered_Maps.Map;
				Translations	: in out Templates_Parser.Translate_Set
			);
	-- This processor produces modules instances and se tehe Translations set so our callback can
	-- printout the resulting page


	type Module_Processor_Type is access procedure(
				Module_Name		: in Unbounded_String;
				Global_Translations	: in Templates_Parser.Translate_Set;
				Output_String		: in out Unbounded_String
			);

	procedure Register_Module_Processor( 
				Name: in Unbounded_String; 
				Processor: in Module_Processor_Type 
			);
	-- Register a module processor to be used in modules called "Name"

	procedure Register_Module_Processor( 
				Name: in String; 
				Processor: in Module_Processor_Type );
	-- Register a module processor to be used in modules called "Name"

private

	-- each module has to implement a function of this type in order to be able to process the page in the Ada side.
	-- Default_Module_Processor (see above) will be used if not.

	procedure Default_Module_Factory(
				Module_Name		: in Unbounded_String;
				Global_Translations	: in Templates_Parser.Translate_Set;
				Output_String		: in out Unbounded_String
			);

	
	package Module_Processor_Maps is new Ada.Containers.Ordered_Maps(
						key_Type	=> Unbounded_String,
						Element_Type	=> Module_Processor_Type
					);

	Module_Processor_Registry: Module_Processor_Maps.Map;

end BeanTeste;
