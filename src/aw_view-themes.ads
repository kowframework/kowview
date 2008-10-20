

---------
-- Ada --
---------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;


---------
-- AWS --
---------

with AWS.Response;
with AWS.Status;
with Templates_Parser;



package Aw_View.Themes is


	type Component_Type is new Aw_View.Components.Component_Interface with private;


	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		);
	-- Initialize the Theme component, setting every variable required



	-------------
	-- Modules --
	-------------


	type Template_Assembler_Module is new Module_Instance_Interface with private;
	-- The template module represents any template
	-- All it does is to load a string representing the AWS template as string
	-- and provide methods for pa


	function Create_Instance(
			Component	: in Template_Assembler_Module;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class;
	-- Creates a module instance



	procedure Initialize_Request(
			Module		: in out Module_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		);
	

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
			Component	: in Component_Type;
			Service_Name	: in String
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


end Aw_View.Components;
