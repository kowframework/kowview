
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
with KOW_Lib.String_Util;
with KOW_Lib.UString_Hashed_Maps;
with KOW_Lib.UString_Vectors;

with KOW_View_Tools.Entities;

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
							Is_Complete_Path	=> True
						);
		App_Config_Map	: KOW_Lib.UString_Hashed_Maps.Map := Get_Contents_Map( App_Config );


		Applications_Tag	: Templates_Parser.Tag;
		Spawn_Tasks_Tag		: Templates_Parser.Tag;
		Entities_Tag		: Templates_Parser.Tag;

		procedure Iterator( C : in KOW_Lib.UString_Hashed_Maps.Cursor ) is
			Complete_Key	: Unbounded_String := KOW_Lib.UString_Hashed_Maps.Key( C );
			
			Element_Values	: KOW_Lib.UString_Vectors.Vector := 
						KOW_Lib.String_Util.Explode( Sep => '.', Str => Complete_Key );
			
			Key		: Unbounded_String := KOW_Lib.UString_Vectors.Last_Element(
								Element_Values
							);
			use Templates_Parser;
		begin
			if Key = "load" then
				if Value( App_Config, To_String( Complete_Key ) ) then
					KOW_Lib.UString_Vectors.Delete_Last( Element_Values );
					Complete_Key := KOW_Lib.String_Util.Implode( '.', Element_Values );
					Applications_Tag := Applications_Tag & Complete_Key;


					if KOW_View_Tools.Entities.Process_Entities( Complete_Key ) then
						Entities_Tag := Entities_Tag & Complete_Key;
					end if;

					if KOW_View_Tools.Entities.Spawn_Tasks( Complete_Key ) then
						Spawn_Tasks_Tag := Spawn_Tasks_Tag & Complete_Key;
					end if;
				end if;
			end if;
		end Iterator;
	begin
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "project_name", P_Name ) );
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "lower_project_name", LP_Name ) );
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "upper_project_name", Ada.Characters.Handling.To_Upper( P_Name ) ) );

		KOW_Lib.UString_Hashed_Maps.Iterate(
				App_Config_Map,
				Iterator'Access
			);

		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "applications", Applications_Tag ) );
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "application_tasks", Spawn_Tasks_Tag ) );
		Templates_Parser.Insert( Parameters, Templates_Parser.Assoc( "entities", Entities_Tag ) );


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
