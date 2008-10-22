



---------
-- AWS --
---------
with AWS;
with AWS.Response;
with AWS.Status;



--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

package Aw_View.Service_Mapping is



	type Service_Map_Type is record
		Component_Name	: Unbounded_String;
		Service_Name	: Unbounded_String;
	end record;


	package Service_Maps is new Ada.Containers.Ordered_Maps(
				Key_Type	=> Unbounded_String,
				Element_Type	=> Service_Map_Type
			);

	procedure Reload_Mappings;


	function AWS_Callback( Request : in AWS.Status.Data ) return AWS.Response.Data;


private
	Mapping		: Service_Maps.Map;
	Default_Service	: Unbounded_String; -- "/page"

end Aw_View.Service_Mapping;
