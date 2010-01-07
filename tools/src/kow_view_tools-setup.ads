


with KOW_View_Tools.Commands;


-----------------------------------------
-- Implementation of the Setup command --
-----------------------------------------

package KOW_View_Tools.Setup is


	type Command_Type is new KOW_View_Tools.Commands.Command_Type with null record;


	function New_Command return KOW_View_Tools.Commands.Command_Type'Class;
	-- constructor for our command

	overriding
	procedure Run( Command : in out Command_Type );
	-- update the [PROJECT_NAME]_Setup procedure so it can be built..
	


	overriding
	procedure Help( Command : in out Command_Type );
	-- show detailed information about this command
	


	overriding
	procedure Describe( Command : in out Command_Type );
	-- show a short description about this command;
end KOW_View_Tools.Setup;
