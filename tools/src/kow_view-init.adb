


--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Text_IO;




-------------------
-- KOW Framework --
-------------------
with KOW_Lib.UString_Vectors;
with KOW_View.Commands;
with KOW_View.Driver;



---------
-- AWS --
---------
with Templates_Parser;

----------------------------------------
-- Implementation of the Init command --
----------------------------------------

package body KOW_View.Init is


	function New_Command return KOW_View.Commands.Command_Type'Class is
		Command : Command_Type;
	begin
		
		return Command;
	end New_Command;



	overriding
	procedure Run( Command : in out Command_Type ) is
		-- initialize the project
		-- required parameter :: project name

		use Ada.Directories;
		use KOW_Lib.UString_Vectors;


		To_Process		: Vector;
		-- list the directories that need processing..
		To_Process_C		: Cursor;
		Current_Directory	: Unbounded_String;

		Directories	: Vector;
		Ordinary_Files	: Vector;
		In_Files	: Vector;



		In_Parameters	: Templates_Parser.Translate_Set;

		Skel_Path	: constant Unbounded_String := To_Unbounded_String( "/etc/kvdriver/skel" );




		function Destination_Path( Name : in String ) return String is
			First : Integer := Length( Skel_Path ) - Name'First;
			Ret : String := Name( First .. Name'Last );
		begin
			if Ret( Ret'First ) = '/' then
				return Ret( Ret'First + 1 .. Ret'Last );
			else
				return Ret;
			end if;
		end Destination_Path;

		function Destination_Path( Name : in Unbounded_String ) return String is
		begin
			return Destination_Path( To_String( Name ) );
		end Destination_Path;


		procedure Process( Directory_Entry : in Directory_Entry_Type ) is
			SName : constant String := Full_Name( Directory_Entry );
			Name : constant Unbounded_String := To_Unbounded_String( SName );
		begin
			case Kind( Directory_Entry ) is
				when Directory =>
					Append( To_Process, Name );
				when Ordinary_File =>
					-- the ordinary files might be both .in files or real ordinary files...
					if Ada.Characters.Handling.To_Lower( Extension( SName ) ) = "in" then
						Append( In_Files, Name );
					else
						Append( Ordinary_Files, Name );
					end if;
				when Special_File => null;
					-- we ignore special files...
			end case;
		end Process;




		procedure Directories_Iterator( C : in Cursor ) is
		begin
			Create_Path( Destination_Path( Element( C ) ) );
		end Directories_Iterator;



		procedure Ordinary_Files_Iterator( C : in Cursor ) is
			Name : String := To_String( Element( C ) );
		begin
			Copy_File(
					Source_Name	=> Name,
					Target_Name	=> Destination_Path( Name )
				);
		end Ordinary_Files_Iterator;


	
		
		procedure In_Files_Iterator( C : in Cursor ) is
			use Ada.Text_IO;
			Name		: String := To_String( Element( C ) );
			Values		: String := Templates_Parser.Parse( Name, In_Parameters );

			Destination	: File_Type;
		begin
			Create( Destination, Out_File, Destination_Path( Name( Name'First .. Name'Last - 3 ) ) );
			Put( Destination, Values );
			Close( Destination );
		end In_Files_Iterator;



		function Project_Name return String is
		begin
			return Ada.Command_Line.Argument( 2 );
		end Project_Name;

	begin
		if Ada.Command_Line.Argument_Count /= 2 then
			raise KOW_View.Commands.USAGE_ERROR with "the init command requires a parameter with the project name";
		end if;


		Append( To_Process, Skel_Path );

		To_Process_C := First( To_Process );

		while Has_Element( To_Process_C ) loop
			Current_Directory := Element( To_Process_C );
			Ada.Directories.Search(
					Directory	=> To_String( Current_Directory ),
					Pattern		=> "",
					Process		=> Process'Access
				);
			Delete( To_Process, To_Process_C );
			-- TODO :: check if the following line is really needed...
			To_Process_C := First( To_Process );

			if Current_Directory /= Skel_Path then
				Append( Directories, Current_Directory );
			end if;
		end loop;


		-- create the directory structure...
		Iterate( Directories, Directories_Iterator'Access );

		-- copy all the ordinary files
		Iterate( Ordinary_Files, Ordinary_Files_Iterator'Access );

		-- process all the .in files into ordinary files using AWS' Templates Parser and as variables
		-- 	project_name
		Templates_Parser.Insert( In_Parameters, Templates_Parser.Assoc( "project_name", Project_Name ) );
		Iterate( In_Files, In_Files_Iterator'Access );
	end Run;


	overriding
	procedure Help( Command : in out Command_Type ) is
		-- show detailed information about this command
		use Ada.Command_Line;
		use Ada.Text_IO;
	begin
		Put_Line( "Usage:" );
		Put_Line( Command_Name & " init project_name" );
		New_Line;
		Put_Line( "Initialize the project folder, where ""project_name"" is a valid compilation unit in Ada for your project" );
	end Help;


	overriding
	procedure Describe( Command : in out Command_Type ) is
		-- show a short description about this command;
	begin
		Ada.Text_IO.Put( "initialize the project folder" );
	end Describe;
end KOW_View.Init;
