

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
		);



	-------------
	-- Modules --
	-------------



	type Page_Module is new Module_Instance_Interface with private;
	-- Responsible for managing the page loading.
	-- It's the engine for the Page_Service service.
	-- Can also be used in your own components and pages.



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Page_Module'Class;
	-- Available modules:
	-- 	. page



	overriding
	procedure Process_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- This is the only procedure implemented by the page module.
	-- That's how it's done so there is no need to deal with dynamic allocation



	--------------
	-- Services --
	--------------



	type Page_Service is new Service_Instance_Interface with private;
	-- a service usually represents a module to the external world.
	-- the service can be mapped to a base URI
	--      . when mapped to /do, /do/something will call it



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String
		) return Service_Instance_Interface'Class;
	-- Available services:
	-- 	. page
	-- 	. static




	overriding
	procedure Process_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- This service provides direct access to the page module.



	overriding
	procedure Process_Request(
			Service		: in out Static_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- This service acts like a standard web server, providing access
	-- to static files.


end Aw_View.Pages;
