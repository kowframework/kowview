



with KOW_View_Tools.Commands;

with KOW_View_Tools.Help;
with KOW_View_Tools.Info;
with KOW_View_Tools.Init;
with KOW_View_Tools.Install_Tool;
with KOW_View_Tools.NewApp;
with KOW_View_Tools.Setup;
with KOW_View_Tools.Version;

-- Contains some tools for managing KOW_View_Tools Projects --
package KOW_View_Tools.Driver is


	-- a enum type used to trac all Commands available.
	type Available_Commands is (
			help,
			info,
			init,
			install_tool,
			newapp,
			setup,
			version
		);



	procedure Run_Command( Command : in Available_Commands );

	function Get( Command : in Available_Commands ) return KOW_View_Tools.Commands.Command_type'Class;

private
	type Available_Commands_Array is array( Available_Commands'First .. Available_Commands'Last ) of access function return KOW_View_Tools.Commands.Command_Type'Class;

	
	Command_Constructors : constant Available_Commands_Array := (
						Help		=> KOW_View_Tools.Help.New_Command'Access,
						Info		=> KOW_View_Tools.Info.New_Command'Access,
						Init		=> KOW_View_Tools.Init.New_Command'Access,
						Install_Tool	=> KOW_View_Tools.Install_Tool.New_Command'Access,
						newapp		=> KOW_View_Tools.NewApp.New_Command'Access,
						Setup		=> KOW_View_Tools.Setup.New_Command'Access,
						Version		=> KOW_View_Tools.Version.New_Command'Access
					);


end KOW_View_Tools.Driver;
