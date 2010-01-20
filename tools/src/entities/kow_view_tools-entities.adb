




--------------
-- Ada 2005 --
--------------
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;



-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Text;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Ordered_Maps;
with KOW_Lib.UString_Vectors;


---------
-- AWS --
---------
with Templates_Parser;

package body KOW_View_Tools.Entities is


	Sep	: constant Character := KOW_Lib.File_System.Separator;

	function Template_Path( Tpl : in String; Ext: in String ) return String is
	begin
		return Ent_Skel_Path & Sep & Tpl & '.' & Ext & ".tpl";
	end Template_Path;



	function Process_Property( Entity : in String; Cfg : in KOW_Config.Config_File ) return String is
		use Templates_Parser;
		use KOW_Lib.UString_Ordered_Maps;

		Parameters	: Templates_Parser.Translate_Set;
		Contents	: Map := KOW_Config.Get_Contents_Map( Cfg );

		procedure Iterator( C : in Cursor ) is
		begin
			Insert( Parameters, Assoc( To_String( Key( C ) ), To_String( Element( C ) ) ) );
		end Iterator;
	begin


		Templates_Parser.Insert( Parameters, Assoc( "entity", Entity ) );

		return Templates_Parser.Parse(
				Template_Path( KOW_Config.Element( Cfg, "template" ) & ".reg", "adb" ), 
				Parameters
			);
	end Process_Property;






	function Process_Entities( Application : Unbounded_String ) return Boolean is
		Cfg : KOW_Config.Config_File;


		App_Str : String := To_String( Application );

		App_Path : String := "applications" & Sep & App_Str & Sep;
		
		App_Cfg : String := App_Path & "application.cfg";
		Ent_Cfg : String := App_Path & "entities.cfg";


		The_Parser : aliased KOW_Config.Text.Parser;




		-- Templates Parser Stuff --

		Parameters		: Templates_Parser.Translate_Set;

		Entities_Tag		: Templates_Parser.Tag;	-- 1d tag
		Id_Generators_Tag	: Templates_Parser.Tag; -- 1d tag
		Table_Names_Tag		: Templates_Parser.Tag; -- 1d tag
		Properties_Tag		: Templates_Parser.Tag; -- 2d tag




		procedure Process_Entity( C : in KOW_Lib.UString_Vectors.Cursor ) is
			-- process the entity whose name is pointed by the cursor...
			
			use KOW_Config;
			use Templates_Parser;
			Entity : String := Ada.Strings.Fixed.Trim( To_String( KOW_Lib.UString_Vectors.Element( C ) ), Ada.Strings.Both );
			Table_Name : String := Element( Cfg, Entity & ".table_name" );
			function Id_Generator return String is
				IDG	: String := Value( Cfg, Entity & ".id_generator", "null" );
			begin
				if IDG = "null" then
					return IDG;
				else
					return IDG & "'Access";
				end if;
			end ID_Generator;


			Properties : Config_File_Array := Elements_Array( Cfg, Entity & ".properties" );
			Entity_Properties_Tag	: Templates_Parser.Tag;
		begin

			
			for i in Properties'Range loop
				Entity_Properties_Tag := Entity_Properties_Tag & Process_Property( Entity, Properties( i ) );
			end loop;
				

			Table_Names_Tag		:= Table_Names_Tag & Table_Name;
			ID_Generators_Tag	:= ID_Generators_Tag & Id_Generator;

			Properties_Tag		:= Properties_Tag & Entity_Properties_Tag;

		end Process_Entity;


		procedure Process_Entities( Entity_Str : in String ) is
			

			V : KOW_Lib.UString_Vectors.Vector :=
				KOW_Lib.String_Util.Explode( ',', Entity_Str );
		begin
			KOW_Lib.UString_Vectors.Iterate( V, Process_Entity'Access );
		end Process_Entities;

	begin

		Cfg := KOW_Config.New_Config_File(
						N	=> App_Cfg,
						P	=> The_Parser'Unchecked_Access
				);

		if KOW_Config.Value( Cfg, "process_entities", False ) then
			Cfg := KOW_Config.New_Config_File(
						N	=> Ent_Cfg,
						P	=> The_Parser'Unchecked_Access
				);


			Process_Entities( KOW_Config.Element( Cfg, "entities" ) );
		end if;


		return true;
	exception
		when KOW_Config.FILE_NOT_FOUND =>
			return false;
	end process_entities;

end KOW_View_Tools.Entities;
