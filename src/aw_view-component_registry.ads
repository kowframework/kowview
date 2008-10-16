
---------
-- Ada --
---------


with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_Config.Generic_Registry;
with Aw_View.Components;		use Aw_View.Components;



package Aw_View.Component_Registry is

	----------------
	-- Exceptions --
	----------------


	COMPONENT_CONFIGURATION_ERROR	: Exception;
	DUPLICATED_COMPONENT_ERROR	: Exception;
	UNKNOWN_COMPONENT_ERROR		: Exception;


	--------------------------
	-- Component Management --
	--------------------------

	procedure Register(
			Component_Name		: in String;
			Component		: in Aw_View.Components.Component_Access;
			Require_Configuration	: in Boolean
			);
	-- A component, once registered, is never deallocated.
	-- All components should be registered at startup.
	--
	-- This procedure also calls "Initialize" for each component. Is also responsible for locating
	-- the component's configuration file.
	--
	-- If Require_Configuration == true and there is no config file available raise 
	-- COMPONENT_CONFIGURATION_ERROR

	function Load( Component_Name: in String ) return Aw_View.Components.Component_Access;
	-- Loads a component by it's name
	-- There is only one instance for each component.


	-----------------------
	-- Module Management --
	-----------------------

	function Load_Module(
			Component_Name	: in String;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class;
	-- get a module instance

	------------------------
	-- Service Management --
	------------------------

	function Load_Service( Component_Name, Service_Name: in String ) return Service_Instance_Interface'Class;
	-- load a service by it's component name and it's name


private


	package Component_Maps is new Ada.Containers.Ordered_Maps(
					Key_Type	=> Unbounded_String,
					Element_Type	=> Component_Access
				);

	The_Registry: Component_Maps.Map;



end Aw_View.Component_Registry;
