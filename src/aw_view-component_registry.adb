

---------
-- Ada --
---------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_Config.Text;
with Aw_Config.Generic_Registry;
with Aw_Lib.File_System;
with Aw_View.Components;		use Aw_View.Components;



package body Aw_View.Component_Registry is


	Parser: Aw_Config.Parser_Access := new Aw_Config.Text.Parser;

	--------------------------
	-- Component Management --
	--------------------------

	procedure Register(
			Component_Name		: in String;
			Component		: in Aw_View.Components.Component_Access;
			Require_Configuration	: in Boolean
			) is
		-- A component, once registered, is never deallocated.
		-- All components should be registered at startup.
		--
		-- This procedure also calls "Initialize" for each component. Is also responsible for locating
		-- the component's configuration file.
		--
		-- If Require_Configuration == true and there is no config file available raise
		-- COMPONENT_CONFIGURATION_ERROR
		Config	: Aw_Config.Config_File;
		CN	: Unbounded_String := To_Unbounded_String( Component_Name );

		use Component_Maps;
	begin
		begin
			Config := Aw_Config.New_Config_File(
					N => "awconfig" & Aw_Lib.File_System.Separator & Component_Name,
					P => Parser
				);
			Initialize( Component.all, Component_Name, Config );
		exception
			when Aw_Config.File_Not_Found =>
				if Require_Configuration then
					raise COMPONENT_CONFIGURATION_ERROR with "Missing config for " & Component_Name;
				end if;
		end;

		-- the component is in the memory and is initialized:
		if Contains( The_Registry, CN ) then
			raise DUPLICATED_COMPONENT_ERROR with Component_Name;
		end if;

		Include( The_Registry, CN, Component );
		
	end Register;



	function Load( Component_Name: in String ) return Aw_View.Components.Component_Access is
		-- Loads a component by it's name
		-- There is only one instance for each component.
	begin
		return Component_Maps.Element( The_Registry, To_Unbounded_String( Component_Name ) );
	exception
		when CONSTRAINT_ERROR =>
			raise UNKNOWN_COMPONENT_ERROR with Component_Name;
	end Load;


	-----------------------
	-- Module Management --
	-----------------------

	function Load_Module(
			Component_Name	: in String;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class is
		-- get a module instance
		Component	: Component_Access := Load( Component_Name );
	begin
		return Create_Instance( Component.all, Module_Name, Config );
	end Load_Module;

	------------------------
	-- Service Management --
	------------------------

	function Load_Service( Component_Name, Service_Name: in String ) return Service_Instance_Interface'Class is
		-- load a service by it's component name and it's name
	begin
		return Create_Instance(
				Load( Component_Name ).all,
				Service_Name
			);
	end Load_Service;


end Aw_View.Component_Registry;
