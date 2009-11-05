



with KOW_View.Driver_Commands;

-- Contains some tools for managing KOW_View Projects --
package KOW_View.Driver is


	-- a enum type used to trac all Commands available.
	type Available_Commands is (
			help,
			version
		);

	Usage_Error : Exception;


	procedure Run_Command( Command : in Available_Commands );

	procedure Register( Command : in Available_Commands; Constructor : function return KOW_View.Driver_Commands.Command_Type'Class );


	function Get( Command : in Available_Commands ) return KOW_View.Driver_Commands.Command_type'Class;

private
	type Command_Constructor is access function return KOW_View.Driver_Commands'Class;

	type Available_Commands_Array is array( Available_Commands'First .. Available_Commands'Last ) of function return KOW_View.Driver_Commands.Command_Type'Class;
	
	Commands_List : Available_Commands_Array;


end KOW_View.Driver;
