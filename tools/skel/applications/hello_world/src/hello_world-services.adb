




with KOW_Lib.Json;
with KOW_View.Components;
with KOW_View.Services;
with KOW_View.Services.Singleton_Service_Cycles;

with AWS.Response;
with AWS.Status;


package body Hello_World.Services is


	protected incrementor is
		procedure increment( C : in out Natural );
	end incrementor;


	overriding
	procedure Process_Custom_Request(
				Service	: in out Hello_Service;
				Request	: in     AWS.Status.Data;
				Response:    out AWS.Response.Data
			) is
	begin
		Incrementor.Increment( Service.Counter );

		Response := AWS.Response.Build( "text/html", "<html><body>Accessed " & Natural'Image( Service.Counter ) & " times!" );
	end Process_Custom_Request;
	
	overriding
	procedure Process_Json_Request(
				Service	: in out Hello_Service;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			) is
		O : KOW_Lib.Json.Object_Type;
	begin
		Incrementor.Increment( Service.Counter );
		KOW_Lib.Json.Set( O, "counter", Service.Counter );
		Response := O;
	end Process_Json_Request;
	
	protected body incrementor is
		procedure increment( C : in out Natural ) is
		begin
			C := C + 1;
		end increment;
	end incrementor;
end Hello_World.Services;
