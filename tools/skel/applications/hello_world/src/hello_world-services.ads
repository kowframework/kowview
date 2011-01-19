




with KOW_Lib.Json;
with KOW_View.Components;
with KOW_View.Services;
with KOW_View.Services.Singleton_Service_Cycles;

with AWS.Response;
with AWS.Status;


with Hello_World.Components;
package Hello_World.Services is


	type Hello_Service is new KOW_View.Services.Service_Type with record
		Counter	: Natural := 0;
	end record;


	overriding
	procedure Process_Custom_Request(
				Service	: in out Hello_Service;
				Request	: in     AWS.Status.Data;
				Response:    out AWS.Response.Data
			);
	
	overriding
	procedure Process_Json_Request(
				Service	: in out Hello_Service;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			);
	
	package Hello_Cycles is new KOW_View.Services.Singleton_Service_Cycles(
					Service_type	=> Hello_Service,
					Component	=> Hello_World.Components.Component'Access
				);

end Hello_World.Services;
	
