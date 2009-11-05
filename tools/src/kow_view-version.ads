


with KOW_View.Commands;


-------------------------------------------
-- Implementation of the Version command --
-------------------------------------------

package KOW_View.Version is


	type Command_Type is new KOW_View.Commands.Command_Type with null record;


	function New_Command return KOW_View.Commands.Command_Type'Class;
	-- constructor for our command

	overriding
	procedure Run( Command : in out Command_Type );
	-- when no parameter is given, display the version message
	


	overriding
	procedure Help( Command : in out Command_Type );
	-- show detailed information about this command
	


	overriding
	procedure Describe( Command : in out Command_Type );
	-- show a short description about this command;
end KOW_View.Version;
