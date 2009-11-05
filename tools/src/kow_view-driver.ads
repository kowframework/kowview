



with KOW_View.Commands;


with KOW_View.Help;
with KOW_View.Version;

-- Contains some tools for managing KOW_View Projects --
package KOW_View.Driver is


	-- a enum type used to trac all Commands available.
	type Available_Commands is (
			help,
			version
		);

	Usage_Error : Exception;


	procedure Run_Command( Command : in Available_Commands );

	function Get( Command : in Available_Commands ) return KOW_View.Commands.Command_type'Class;

private
	type Available_Commands_Array is array( Available_Commands'First .. Available_Commands'Last ) of access function return KOW_View.Commands.Command_Type'Class;

	
	Command_Constructors : constant Available_Commands_Array := (
						Help	=> KOW_View.Help.New_Command'Access,
						Version	=> KOW_View.Version.New_Command'Access
					);


end KOW_View.Driver;
