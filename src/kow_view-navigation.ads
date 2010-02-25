

---------
-- Ada --
---------
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with KOW_Config;
with KOW_View.Components;		use KOW_View.Components;

---------
-- AWS --
---------

with AWS.Response;
with AWS.Session;
with AWS.Status;
with Templates_Parser;



package KOW_View.Navigation is

	----------------
	-- Components --
	----------------


	type Component_Type is new KOW_View.Components.Component_Interface with private;

	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		);
	-- Initialize the component while starting up the server
	-- Config is an already initialized configuration file located at:
	-- 	kowview/component_name
	--
	-- Configuration Parameters:
	-- 	login_error_page	:: default "/theme/login"
	--	Access_Denied_page	:: default "/theme/403"
	-- 	default_redirect	:: default "/"


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return Module_Instance_Interface'Class;
	-- no matter what module we request, the Menu_Module_Type  will always be created


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class;
	-- there is no service, so.. an exception is raised


	-------------
	-- Modules --
	-------------

	type Menu_Module_Type is new Module_Instance_Interface with private;
	-- a module is something that can be accessed anywhere inside the system.



	overriding
	procedure Process_Request(
			Module		: in out Menu_Module_Type;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- print the menu


private

	type Component_Type is new KOW_View.Components.Component_Interface with record
		Default_Menu_Template	: Unbounded_String	:= To_Unbounded_String( "default_menu" );
		-- TODO: definir default_link_template
	end record;


	type Link_Descriptor_Type is record
		Label, Href	: Unbounded_String;
		Level		: Natural;
	end record;

	package Link_Vectors is new Ada.Containers.Vectors(
					Element_Type	=> Link_Descriptor_Type,
					Index_Type	=> Natural
				);

	type Menu_Module_Type is new Module_Instance_Interface with record
		Links		: Link_Vectors.Vector;
		Template	: Unbounded_String;
	end record;
end KOW_View.Navigation;
