



with KOW_View.Services;			use KOW_View.Services;



package My_Component.Services is



	type Echo_Service is new KOW_View.Services.Service_Type with null record;

	package Echo_Factories is new KOW_View.Services.Singleton_Factories(
						Service_Type	=> Echo_Service,
						Component	=> My_Component.Self	-- it's a redundancy; should it be removed?
					);



	overriding
	procedure Initialize(
				Service	: in out Echo_Service
			);
	-- this will be called only when the service is allocated
	-- as kowview uses only access types, we use ada.finalization.controlled type


	overriding
	procedure Process_Custom_Request(
				Service	: in out Echo_Service;
				Status	: in     KOW_View.Request_Status_Type;
				Response:    out AWS.Response.Data
			);


	overriding
	procedure Process_Json_Request(
				Service	: in out Echo_Service;
				Status	: in     KOW_VIew.Request_Status_Type;
				Response:    out KOW_Lib.Json.Object_Type
			);
				

end My_Component.Services;
