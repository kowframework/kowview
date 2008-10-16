

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



package Aw_View.Components.Security is

	----------------
	-- Components --
	----------------

	type Component_Type is new Aw_View.Components.Component_Type with private;
	


	overriding
	procedure Initialize(
			Component	: in     Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		);
	-- Initialize the component while starting up the server
	-- Config is an already initialized configuration file located at:
	-- 	awview/component_name
	--
	-- Configuration Parameters:
	-- 	login_error_page	:: default "/theme/login"
	--	Access_Denied_page	:: default "/theme/403"
	-- 	default_redirect	:: default "/"


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module		: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class;
	-- no matter what module we request, the Criteria_Module_Module will be always called


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service		: in String
		) return Service_Instance_Interface'Class;


	-------------
	-- Modules --
	-------------

	type Criteria_Module is new Module_Instance_Interface with private;
	-- a module is something that can be accessed anywhere inside the system.




	overriding
	procedure Initialize_Request(
			Module		: in out Criteria_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		);
	-- if the user can access the page, do nothing.
	-- if it can't, then build a 'Location: access_denyed_page


	procedure Process_Header(
			Module		: in out Module_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- used only for debugging
	-- print the expression criteria used as a HTML comment



	--------------
	-- Services --
	--------------
	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service		: in String
		) return Service_Instance_Interface'Class;



	type Login_Service is new Service_Instance_Interface with private;
	-- try to login the user and redirect to the correct status page
	procedure Process_Request(
			Service		: in out Logout_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);


	type Logout_Service is new Service_Instance_Interface with private;
	-- process the logout, if required, and redirect to some standard page


	procedure Process_Request(
			Service		: in out Logout_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- process a request to a service
	-- the entire request is handled by the service
	-- sometimes is useful for a service only to be created and released - such as in a counter service

private

	type Component_Type is new Aw_View.Components.Component_Type with record
		Default_Redirect	: Unbounded_String	:= To_Unbounded_String( "/" );
		Access_Denied_Page	: Unbounded_String	:= To_Unbounded_String( "/theme/403" );
		Login_Error_Page	: Unbounded_String	:= To_Unbounded_String( "/theme/login" );
	end Component_Type;


	type Criteria_Module is new Module_Instance_Interface with record
		Expression		: Unbounded_String;
	end record;


	type Login_Service is new Service_Instance_Interface with record
		Default_Redirect	: Unbounded_String;
		Login_Error_Page	: Unbounded_String;
	end record;

	type Logout_Service is new Service_Instance_Interface with record
		Default_Redirect	: Unbounded_String;
	end record;

end Aw_View.Components.Security;
