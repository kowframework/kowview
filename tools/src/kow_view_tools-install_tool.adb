



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


	-----------------------------------------------------------------
	-- Main Procedure so it can be also called by other procedures --
	-----------------------------------------------------------------

	
	procedure Install( Tool : in String ) is
		-- install the given tool
		Tpl_Parameters	: Templates_Parser.Translate_Set;


		PN	: constant String := Project_Name;
		LPN	: constant String := Ada.Characters.Handling.To_Lower( PN );
		UPN	: constant String := Ada.Characters.Handling.To_Upper( PN );
	begin
		if Project_name = "" then
			raise KOW_View_Tools.Commands.Usage_ERROR with "you need to run this within an existing project directory";
		end if;

		Templates_Parser.Insert( Tpl_Parameters, Templates_Parser.Assoc( "project_name", PN ) );
		Templates_Parser.Insert( Tpl_Parameters, Templates_Parser.Assoc( "lower_project_name", LPN ) );
		Templates_Parser.Insert( Tpl_Parameters, Templates_Parser.Assoc( "upper_project_name", UPN ) );

	
		declare
			Name		: String := Tools_Skel_Path & "/project_name_" & Tool & ".adb.tpl";
			Destination_Path: String := "./tools-src/" & LPN & '_' & Tool & ".adb";

			Destination	: File_Type;
		begin
			if not Ada.Directories.Exists( Name ) then
				raise KOW_View_Tools.Commands.Usage_ERROR with "There is no such tool: " & Tool;
			end if;

			if Ada.Directories.Exists( Destination_Path ) then
				Open( Destination, Out_File, Destination_Path );
			else
				Create( Destination, Out_File, Destination_Path );
			end if;
			Put( Destination, Templates_Parser.Parse( Name, Tpl_Parameters ) );
			Close( Destination );
		end;

	end Install;


	------------------
	-- Command Type --
	------------------


	function New_Command return KOW_View_Tools.Commands.Command_Type'Class is
		-- constructor for our command
		Command : Command_Type;
	begin
		return Command;
	end New_Command;

	overriding
	procedure Run( Command : in out Command_Type ) is
		-- when no parameter is given, display the Install_Tool message
	begin

		if Argument_Count /= 2 then
			raise KOW_View_Tools.Commands.Usage_ERROR with "You need to specify a tool to install";
		end if;

		Install( Argument( 2 ) );
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
		Put( "Install a command-line tool into tools-src overwriting any existing file" );
	end Describe;

	
end KOW_View_Tools.Install_Tool;
