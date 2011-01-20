



with KOW_Lib.Json;
with KOW_View.Modules;
with KOW_View.Modules.Stateful_Module_Factories;


with Hello_World.Components;

with Ada.Strings.Unbounded;				use Ada.Strings.Unbounded;
with AWS.Status;


package Hello_World.Modules is


	type Hello_There_Module is new KOW_View.Modules.Module_Type with record
		Counter : Natural := 0;
	end record;


	overriding
	procedure Process_Body(
				Module	: in out Hello_There_Module;
				Request	: in     AWS.Status.Data;
				Response:    out Unbounded_String
			);

	overriding
	procedure Process_Json_Request(
				Module	: in out Hello_There_Module;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			);
	-- return an object with only the "counter" attribute 

	package Hello_There_Factories is new KOW_View.Modules.Stateful_Module_Factories(
				Module_type	=> Hello_There_Module,
				Component	=> Hello_World.Components.Component'Access
			);

end Hello_World.Modules;
