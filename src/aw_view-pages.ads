

---------
-- Ada --
---------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_View.Components;		use Aw_View.Components;
with Aw_View.Themes;

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
		);
	-- the only thing to setup is the theme_component


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class;
	-- Available modules:
	-- 	. page



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
	procedure Initialize_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		);
	-- this is where the page is initialized.


	overriding
	procedure Process_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- it's where the page is assembled.

	overriding
	procedure Finalize_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
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
		Theme_Component_Name: Unbounded_String;
	end record;


	type Page_Module is new Module_Instance_Interface with record
		Config		: Aw_Config.Config_File;
		Processor	: Aw_View.Themes.Template_Processor_Module;
		-- there is no processing of the config file before 
		-- the page rendering begins.
		Theme_Component_Name : Unbounded_String;
	end record;

	type Page_Service is new Service_Instance_Interface with record
		Mapping: Unbounded_String;
	end record;

	type Static_Service is new Service_Instance_Interface with record
		Mapping: Unbounded_String;
	end record;

end Aw_View.Pages;
