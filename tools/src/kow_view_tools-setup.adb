
--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Command_Line;			use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;			use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;			use KOW_Config;
with KOW_Config.Text;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Ordered_Maps;
with KOW_Lib.UString_Vectors;

---------
-- AWS --
---------
with Templates_Parser;




package body KOW_View_Tools.Setup is



	function New_Command return KOW_View_Tools.Commands.Command_Type'Class is
		-- constructor for our command
		Command : Command_Type;
	begin
		return Command;
	end New_Command;


	overriding
	procedure Run( Command : in out Command_Type ) is
		-- update the [PROJECT_NAME]_Setup procedure so it can be built..


		Parameters	: Templates_Parser.Translate_Set;
		P_Name		: constant String := Project_Name;
		LP_Name		: constant String := Ada.Characters.Handling.To_Lower( P_Name );

		App_Config	: Config_File := New_Config_File(
							N			=> "applications.cfg",
							P			=> new KOW_Config.Text.Parser,
							Is_Complete_Path	=> True
						);
		App_Config_Map	: KOW_Lib.UString_Ordered_Maps.Map := Get_Contents_Map( App_Config );


		Components_Tag	: Templates_Parser.Tag;
		Entities_Tag	: Templates_Parser.Tag;

		procedure Iterator( C : in KOW_Lib.UString_Ordered_Maps.Cursor ) is
			Complete_Key	: Unbounded_String := KOW_Lib.UString_Ordered_Maps.Key( C );
			
			Key		: Unbounded_String := KOW_Lib.UString_Vectors.Last_Element(
								KOW_Lib.String_Util.Explode( Sep => '.', Str => Complete_Key )
							);
			High		: Integer;
			use Templates_Parser;
		begin
			Put_Line( To_String( Complete_Key ) );
			if Key = "load_component" then
				if Value( App_Config, To_String( Complete_Key ) ) then
					High := Length( Complete_Key ) - 11;
					Components_Tag := Components_Tag & Unbounded_Slice( 
										Source	=> Complete_Key,
										Low	=> 1,
										High	=> High
									);
				end if;
			elsif Key = "load_entities" then
				if Value( App_Config, To_String( Complete_Key ) ) then
					High := Length( Complete_Key ) - 7;
					Entities_Tag := Entities_Tag & Unbounded_Slice(
										Source	=> Complete_Key,
										Low	=> 1,
										High	=> High
									);
				end if;
			end if;
		end Iterator;
	begin
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "project_name", P_Name ) );
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "lower_project_name", LP_Name ) );
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "upper_project_name", Ada.Characters.Handling.To_Upper( P_Name ) ) );

		KOW_Lib.UString_Ordered_Maps.Iterate(
				App_Config_Map,
				Iterator'Access
			);

		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "entity_packages", Entities_Tag ) );
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "component_packages", Components_Tag ) );


		declare
			Contents	: String := Templates_Parser.Parse( "src/" & LP_Name & "_setup.adb.in", Parameters );
			Destination	: String := "src/" & LP_Name & "_setup.adb";
			F		: File_Type;
		begin
			if Ada.Directories.Exists( Destination ) then
				Open( F, Out_File, Destination );
			else
				Create( F, Out_File, Destination );
			end if;
			Put( F, Contents );
			Close( F );
		end;
	end Run;
	


	overriding
	procedure Help( Command : in out Command_Type ) is
		-- show detailed information about this command
	begin
		Put_Line( "Usage:" );
		Put_Line( Command_Name & " setup" );
		New_Line;
		Put_Line( "Update the setup procedure so it can load all the installed and configured applications." );
		Put_Line( "For more information, read the file ""applications.cfg""." );
	end Help;



	overriding
	procedure Describe( Command : in out Command_Type ) is
		-- show a short description about this command;
	begin
		Put( "update the setup procedure" );
	end Describe;


end KOW_View_Tools.Setup;
