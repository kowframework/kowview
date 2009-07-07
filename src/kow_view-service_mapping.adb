



---------------
-- Ada Works --
---------------

with KOW_Config;
with KOW_Config.Text;
with KOW_View.Components;
with KOW_View.Components_Registry;

---------
-- AWS --
---------
with AWS;
with AWS.Response;
with AWS.Status;



--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

package body KOW_View.Service_Mapping is

	procedure Reload_Mappings is
		use KOW_Config;

		Cfg: Config_File := New_Config_File(
						"kow_view_mappings",
						new KOW_Config.Text.Parser
					);
		Cfgs : Config_File_Array := Elements_Array( Cfg, "map" );
	begin

		Default_Service := Value( cfg, "default_service", "pages" );
		Service_Maps.Clear( Mapping );

		for i in Cfgs'Range loop
			declare
				Map: Service_Map_Type;
			begin
				Map.Component_Name := Element( Cfgs( i ), "component" );
				Map.Service_Name   := Element( Cfgs( i ), "service" );

				Service_Maps.Include(
					Mapping,
					Element( Cfgs( i ), "uri" ),
					Map
				);
			end;
		end loop;
	end Reload_Mappings;


	procedure Run_Request(
			Mapping_Str	: in     String;
			Service_Map	: in out Service_Map_Type;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is

		use KOW_View.Components;
		use KOW_View.Components_Registry;

		Service : Service_Instance_Interface'Class := Load_Service(
				Component_Name	=> To_String( Service_Map.Component_Name ),
				Service_Name	=> To_String( Service_Map.Service_Name ),
				Service_Mapping	=> Mapping_Str
			);
	begin
		Process_Request(
			Service		=> Service,
			Request		=> Request,
			Response	=> Response
		);
	end Run_Request;

	function AWS_Callback( Request : in AWS.Status.Data )
		return AWS.Response.Data is


		Response : AWS.Response.Data;

		URI : constant string := AWS.Status.URI( Request );

		function Last_URI_Boundary return Integer is
		begin

			if URI'Length = 1 then
				Response := AWS.Response.URL(
					Location => To_String( Default_Service )
					);
				return -13;
			end if;


			for i in URI'First + 1 .. URI'Last loop
				if URI( i ) = '/' then
					return i - 1;
				end if;
			end loop;


			-- if got here, the URI is the service mapping:
			return URI'Last;
		
		end Last_URI_Boundary;



		LUB: Integer := Last_URI_Boundary;
	begin
		if LUB = -13 then
			return Response;
		end if;

		declare
			Mapping_Str: String := URI( URI'First .. LUB );
			Service_Map : Service_Map_Type;
		begin
			begin
				Service_Map := Service_Maps.Element(
								Mapping,
								To_Unbounded_String( Mapping_Str )
							);
			exception
				when CONSTRAINT_ERROR =>
					raise PROGRAM_ERROR with "file not found";
			end;

			Run_Request(
				Mapping_Str	=> Mapping_Str,
				Service_Map	=> Service_Map,
				Request		=> Request,
				Response	=> Response
			);

			return Response;
		end;
	
	end AWS_Callback;			

end KOW_View.Service_Mapping;
