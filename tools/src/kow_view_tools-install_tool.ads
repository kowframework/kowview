


with KOW_View_Tools.Commands;


------------------------------------------------
-- Implementation of the Install_Tool command --
------------------------------------------------

package KOW_View_Tools.Install_Tool is


	-----------------------------------------------------------------
	-- Main Procedure so it can be also called by other procedures --
	-----------------------------------------------------------------

	
	procedure Install( Tool : in String );
	-- install the given tool

	------------------
	-- Command Type --
	------------------


	type Command_Type is new KOW_View_Tools.Commands.Command_Type with null record;


	function New_Command return KOW_View_Tools.Commands.Command_Type'Class;
	-- constructor for our command

	overriding
	procedure Run( Command : in out Command_Type );
	-- when no parameter is given, display the Install_Tool message
	


	overriding
	procedure Help( Command : in out Command_Type );
	-- show detailed information about this command
	


	overriding
	procedure Describe( Command : in out Command_Type );
	-- show a short description about this command;
end KOW_View_Tools.Install_Tool;
