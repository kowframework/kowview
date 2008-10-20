

---------
-- Ada --
---------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_View.Components;		use Aw_View.Components;

---------
-- AWS --
---------

with AWS.Response;
with AWS.Status;
with Templates_Parser;



package Aw_View.Pages is


	----------------
	-- Components --
	----------------

	type Component_Type is new Aw_View.Components.Component_Interface with private;
	-- This component is responsible for calling all other components in order
	-- to render the page.
	--
	-- It's also responsible for calling the Theme component.
	


	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		) is null;
	-- no initialization


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class;
	-- Available modules:
	-- 	. page
	-- 	. static



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class;
	-- Available services:
	-- 	. page
	-- 	. static



	-------------
	-- Modules --
	-------------



	--
	-- Page Module
	--


	type Page_Module is new Module_Instance_Interface with private;
	-- Responsible for managing the page loading.
	-- It's the engine for the Page_Service service.
	-- Can also be used in your own components and pages.



	overriding
	procedure Process_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- This is the only procedure implemented by the page module.
	-- That's how it's done so there is no need to deal with dynamic allocation


	--
	-- Static Module
	--


	type Static_Module is new Module_Instance_Interface with private;
	-- Responsible for managing static content loading.
	-- It should be used to read from static text files into the rendered page.
	--
	-- It's not the same thing as the static service!


	overriding
	procedure Process_Request(
			Module		: in out Static_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);


	--------------
	-- Services --
	--------------


	--
	-- Page Service
	--


	type Page_Service is new Service_Instance_Interface with private;
	-- a service usually represents a module to the external world.
	-- the service can be mapped to a base URI
	--      . when mapped to /do, /do/something will call it


	overriding
	procedure Process_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- This service provides direct access to the page module.

	--
	-- Static Service
	-- 

	
	type Static_Service is new Service_Instance_Interface with private;

	overriding
	procedure Process_Request(
			Service		: in out Static_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- This service acts like a standard web server, providing access
	-- to static files.
private


	type Component_Type is new Aw_View.Components.Component_Interface with record
		Default_Mapping: Unbounded_String;
	end record;


	type Page_Module is new Module_Instance_Interface with record
		Config: Aw_Config.Config_File;
		-- there is no processing of the config file before 
		-- the page rendering begins.
	end record;

	type Static_Module is new Module_Instance_Interface with record
		Resource	: Unbounded_String;
		-- the resource name to load

		Extension	: Unbounded_String;
		-- the extension of the resource to load
	end record;

	type Page_Service is new Service_Instance_Interface with record
		Mapping: Unbounded_String;
	end record;

	type Static_Service is new Service_Instance_Interface with record
		Mapping: Unbounded_String;
	end record;

end Aw_View.Pages;
