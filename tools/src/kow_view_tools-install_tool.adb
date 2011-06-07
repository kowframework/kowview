



--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;		use Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_View_Tools.Commands;

---------
-- AWS --
---------
with Templates_Parser;

package body KOW_View_Tools.Install_Tool is


	function New_Command return KOW_View_Tools.Commands.Command_Type'Class is
		-- constructor for our command
		Command : Command_Type;
	begin
		return Command;
	end New_Command;

	overriding
	procedure Run( Command : in out Command_Type ) is
		-- when no parameter is given, display the Install_Tool message
		Tpl_Parameters	: Templates_Parser.Translate_Set;

	begin

		if Argument_Count /= 2 then
			raise KOW_View_Tools.Commands.Usage_ERROR with "You need to specify a tool to install";
		end if;


		if Project_name = "" then
			raise KOW_View_Tools.Commands.Usage_ERROR with "you need to run this within an existing project directory";
		end if;

		Templates_Parser.Insert( Tpl_Parameters, Templates_Parser.Assoc( "project_name", Project_Name ) );
		Templates_Parser.Insert( Tpl_Parameters, Templates_Parser.Assoc( "lower_project_name", Ada.Characters.Handling.To_Lower( Project_Name ) ) );
		Templates_Parser.Insert( Tpl_Parameters, Templates_Parser.Assoc( "upper_project_name", Ada.Characters.Handling.To_Upper( Project_Name ) ) );

	
		declare
			Name		: String := Tools_Skel_Path & '/' & Argument( 2 ) & ".adb.tpl";
			Destination_Path: String := "./tools-src/" & Argument( 2 ) & ".adb";

			Destination	: File_Type;
		begin
			if not Ada.Directories.Exists( Name ) then
				raise KOW_View_Tools.Commands.Usage_ERROR with "There is no such tool: " & Argument( 2 );
			end if;

			if Ada.Directories.Exists( Destination_Path ) then
				Open( Destination, Out_File, Destination_Path );
			else
				Create( Destination, Out_File, Destination_Path );
			end if;
			Put( Destination, Templates_Parser.Parse( Name, Tpl_Parameters ) );
			Close( Destination );
		end;
	end Run;


	overriding
	procedure Help( Command : in out Command_Type ) is
		-- show detailed information about this command
	begin
		Describe( Command );
		New_Line;
	end Help;


	overriding
	procedure Describe( Command : in out Command_Type ) is
		-- show a short description about this command;
	begin
		Put( "Install a command-line tool into tools-src overwriting any existing file. You still need to add this tool to your gpr file" );
	end Describe;

	
end KOW_View_Tools.Install_Tool;
