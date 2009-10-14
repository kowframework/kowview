

with KOW_Config;
with KOW_View.Components;	use KOW_View.Components;



with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;



with AWS.Status;
with Templates_Parser;

package Hello_World is


	type Component_Type is new Component_Interface with record
		Name	: Unbounded_String;
		User	: Unbounded_String;
	end record;

	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		);


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		)
		return Module_Instance_Interface'Class;

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		)
		return Service_Instance_Interface'Class;

	-- the echo module:
	

	type Echo is new Module_Instance_Interface with record
		Message	: Unbounded_String;
	end record;


	overriding
	procedure Process_Request(
			Module		: in out Echo;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);


	type void is new service_instance_interface with null record;
end Hello_World;
