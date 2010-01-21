




--------------
-- Ada 2005 --
--------------
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Text_IO;



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

	function "+"(L,R : in String ) return String is
	begin
		return L & Sep & R;
	end "+";


	function Template_Path( Tpl : in String; Ext: in String ) return String is
	begin
		return Ent_Skel_Path & Tpl & '.' & Ext & ".tpl";
	end Template_Path;



	function Entity_File_Destination_Path(
				Application	: in String;
				Entity		: in String;
				Property	: in String
			) return String is
		-- calculate the destination path for the files in the form:
		-- 	./applications/APPLICATION/entities-src/application-entities-property_hlp
	begin
		return "applications" + Application + "entities-src" + application & "-entities-" & property & "_hlp";
	end Entity_File_Destination_Path;

	function Process_Property( Application : in String; Entity : in String; Cfg : in KOW_Config.Config_File ) return String is
		use Templates_Parser;
		use KOW_Lib.UString_Ordered_Maps;

		Parameters	: Templates_Parser.Translate_Set;
		Contents	: Map := KOW_Config.Get_Contents_Map( Cfg );

		procedure Iterator( C : in Cursor ) is
		begin
			Insert( Parameters, Assoc( To_String( Key( C ) ), To_String( Element( C ) ) ) );
		end Iterator;


		Template : String := KOW_Config.Element( Cfg, "template" );

	begin


		Templates_Parser.Insert( Parameters, Assoc( "entity", Entity ) );

		Iterate( Contents, Iterator'Access );



		if not KOW_Config.Has_Element( Cfg, "getter" ) and then not KOW_Config.Has_Element( Cfg, "setter" ) then
			declare
				use Ada.Text_IO;
				Property : String := KOW_Config.Element( Cfg, "property" );
				F_Path : String := Entity_File_Destination_Path( Application, Entity, property);
				T_Ads_Path : String := Template_Path( Template, "ads" );
				T_Adb_Path : String := Template_Path( Template, "adb" );


				Dest_Pkg : String := Application & ".Entities." & Property & "_hlp.";

				procedure doit( From, To : in String ) is
					F : File_Type;

					Cont : String := Templates_Parser.Parse( From, Parameters);
				begin
					if Ada.Directories.Exists( To ) then
						Open( F, Out_File, To );
					else
						Create( F, Out_File, To );
					end if;

					Put( F, Cont );

					Close( F );
				end doit;
					
			begin
				Templates_Parser.Insert(
						Parameters,
						Assoc(
								"getter",
								Dest_Pkg & "getter"
							)
					);
				Templates_Parser.Insert(
						Parameters,
						Assoc(
								"setter",
								Dest_Pkg & "setter"
							)
					);

				doit( From => T_Ads_Path, To => F_Path & ".ads" );
				doit( From => T_Adb_Path, To => F_Path & ".adb" );

			end;
		end if;

		return Templates_Parser.Parse(
				Template_Path( Template & ".reg", "adb" ), 
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
				Entity_Properties_Tag := Entity_Properties_Tag & Process_Property( To_String( Application ), Entity, Properties( i ) );
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


		declare
			procedure  doit( Ext : in String ) is
				use Ada.Text_IO;
				Cont : String := Templates_Parser.Parse(
							Template_Path(
								"application-entity_setup",
								Ext
							)
						);
				F : File_Type;

				Dest : String := "applications" + "entities-src" + to_string(application) & "-entities_setup." & Ext;
			begin
				if Ada.Directories.Exists( Dest ) then
					Open( F, Out_File, Dest );
				else
					Create( F, Out_File, Dest );
				end if;
				Put( F, Cont );
				Close( F );
			end doit;
		begin
			doit( "ads" );
			doit( "adb" );
		end;
				
		

		return true;
	exception
		when KOW_Config.FILE_NOT_FOUND =>
			return false;
	end process_entities;

end KOW_View_Tools.Entities;
