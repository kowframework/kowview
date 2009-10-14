

---------
-- Ada --
---------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with KOW_Config;


---------
-- AWS --
---------

with AWS.Response;
with AWS.Status;
with Templates_Parser;



package KOW_View.Components is


	----------------
	-- Exceptions --
	----------------
	COMPONENT_ERROR	: Exception;
	MODULE_ERROR	: Exception;
	SERVICE_ERROR	: Exception;


	----------------
	-- Components --
	----------------

	type Component_Interface is abstract tagged null record;
	-- it encapsulates a set of functionalities provided by means of modules and services
	-- there should be only one instance of this type each time.
	
	type Component_Access is not null access all Component_Interface'Class;


	procedure Initialize(
			Component	: in out Component_Interface;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		) is abstract;
	-- Initialize the component while starting up the server
	-- Config is an already initialized configuration file located at:
	-- 	kowview/component_name



	-------------
	-- Modules --
	-------------



	type Module_Instance_Interface is interface;
	-- a module is something that can be accessed anywhere inside the system.

	type Module_Instance_Access is not null access all Module_Instance_Interface'Class;





	function Create_Instance(
			Component	: in Component_Interface;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File 
		) return Module_Instance_Interface'Class is abstract;
	-- create a new module instance.
	-- depending on the service, the instance object can represent different things and can, or not, even me extended
	-- to implement additional functionality.
	-- A service can also have it's own state which can be saved in the session for later retrieval.



	procedure Initialize_Request(
			Module		: in out Module_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		) is null;
	-- Initialize the processing of a request
	-- Called before anything has been build.
	-- If Is_Final = True than stop processing other modules and return Response
	-- Useful when handling secured modules or modules that require sending cookies

	procedure Process_Header(
			Module		: in out Module_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is null;
	-- process header of the response.
	-- it's assumed that 

	procedure Process_Request(
			Module		: in out Module_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is null;
	-- process the request for a module.
	-- sometimes is useful for a module only to be created and released - such as in a page counter module

	procedure Process_Footer(
			Module		: in out Module_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is null;
	-- process some footer of the module
	-- useful when creating benchmar modules

	procedure Finalize_Request(
			Module		: in out Module_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
		) is null;
	-- Finalize processing the request.
	-- Called when the process has been finalized



	--------------
	-- Services --
	--------------



	type Service_Instance_Interface is interface;
	-- a service usually represents a module to the external world.
	-- the service can be mapped to a base URI
	--      . when mapped to /do, /do/something will call it

	type Service_Instance_Access is not null access all Service_Instance_Interface'Class;




	function Create_Instance(
			Component	: in Component_Interface;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class is abstract;
	-- create a new service instance.
	-- depending on the service, the instance object can represent different things and can, or not, even me extended
	-- to implement additional functionality.
	-- A service can also have it's own state which can be saved in the session for later retrieval.
	--
	-- The service configuration should be handled by the Component Initialization




	procedure Process_Request(
			Service		: in out Service_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is null;
	-- process a request to a service
	-- the entire request is handled by the service
	-- sometimes is useful for a service only to be created and released - such as in a counter service


end KOW_View.Components;