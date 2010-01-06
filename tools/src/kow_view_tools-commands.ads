

------------------------------------------------
-- This is the home for the command interface --
------------------------------------------------

package KOW_View_Tools.Commands is



	Usage_Error : Exception;

	type Command_Type is abstract tagged null record;

	procedure Run( Command : in out Command_Type ) is abstract;
	-- execute the command
	

	procedure Help( Command : in out Command_Type ) is abstract;
	-- print the detailed help message about the command (used by the help command)
	

	procedure Describe( Command : in out Command_Type ) is abstract;
	-- print a short description about the command (used by the help command)

end KOW_View_Tools.Commands;
