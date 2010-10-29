

---------
-- Ada --
---------
with Ada.IO_Exceptions;
with Ada.Directories;			use Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

---------------
-- Ada Works --
---------------

with KOW_Config;
with KOW_Config.Generic_Registry;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;		use KOW_View.Components;



package body KOW_View.Components_Registry is


	--------------------------
	-- Component Management --
	--------------------------

	procedure Register(
			Component_Name		: in String;
			Component		: in KOW_View.Components.Component_Access;
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
		CN	: Unbounded_String := To_Unbounded_String( Component_Name );

		use Component_Maps;
	begin
		-- the component is in the memory and is initialized:
		if Contains( The_Registry, CN ) then
			raise DUPLICATED_COMPONENT_ERROR with Component_Name;
		end if;

		Component.all.Require_Configuration := Require_Configuration;
		Component.all.Component_Name := To_Unbounded_String( Component_Name );

		Include( The_Registry, CN, Component );
		
	end Register;


	
	procedure Setup( Component : in out KOW_View.Components.Component_Interface'Class; Component_name : in String ) is
		Config		: KOW_Config.Config_File;
	begin
		Config := Load_Main_Configuration( Component_Name );
		Initialize( Component, Component_Name, Config );
	exception
		when KOW_Config.File_Not_Found =>
			if Component.Require_Configuration then
				raise COMPONENT_CONFIGURATION_ERROR with "Missing config for " & Component_Name;
			else
				-- else we run initialize with an empty config file..
				Initialize( Component, Component_Name, Config );
			end if;

		-- Include( The_Registry, CN, Component );
		-- as we a dealing with access types, no need to replace the component in the map ;)
		
	end Setup;



	procedure Setup( Component_Name : in String ) is
		-- tries to setup the component
		-- if Require_Configuration = false and 

		Config		: KOW_Config.Config_File;
		CN		: Unbounded_String := To_Unbounded_String( Component_Name );
		Component	: KOW_View.Components.Component_Access := Component_Maps.Element( The_Registry, CN );

	begin
		Setup( Component.all, Component_Name );
	end Setup;
	



	procedure Setup_Components is
		-- run setup for every registered component
		use Component_Maps;

		procedure Iterator( C : in Cursor ) is
		begin
			Setup( Element( C ).all, To_String( Key( C ) ) );
		end Iterator;
	begin
		Iterate( The_Registry, Iterator'Access );
	end Setup_Components;




	function Load( Component_Name: in String ) return KOW_View.Components.Component_Access is
		-- Loads a component by it's name
		-- There is only one instance for each component.
	begin
		return Load( To_Unbounded_String( Component_Name ) );
	end Load;


	function Load( Component_Name: in Unbounded_String ) return KOW_View.Components.Component_Access is
	begin
		return Component_Maps.Element( The_Registry, Component_Name );
	exception
		when CONSTRAINT_ERROR =>
			raise UNKNOWN_COMPONENT_ERROR with To_String( Component_Name );
	end Load;


	-----------------------
	-- Module Management --
	-----------------------

	function Load_Module(
			Component_Name	: in Unbounded_String;
			Module_Name	: in Unbounded_String;
			Config		: in KOW_Config.Config_File;
			Module_ID	: in Positive := 1
		) return Module_Instance_Interface'Class is
		-- get a module instance
		Component	: Component_Access := Load( Component_Name );
		Module		: Module_Instance_Interface'Class := Create_Instance( Component.all, To_String( Module_Name ), Config );
	begin
		Module.Module_ID := Module_ID;
		Module.Component := Component_Ptr( Component );

		return Module;
	end Load_Module;


	function Load_Module(
			Component_Name	: in String;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File;
			Module_ID	: in Positive := 1
		) return Module_Instance_Interface'Class is
		-- get a module instance
		Component	: Component_Access := Load( Component_Name );
		Module		: Module_Instance_interface'Class := Create_Instance( Component.all, Module_Name, Config  );
	begin
		Module.Module_ID := Module_ID;
		Module.Component := Component_Ptr( Component );

		return Module;
	end Load_Module;


	function Load_Module(
			Component_Name	: in String;
			Module_Name	: in String;
			Module_ID	: in Positive := 1
		) return Module_Instance_Interface'Class is
		-- get the module, using the standard module configuration
	begin
		return Load_Module(
				Component_Name	=> Component_Name,
				Module_Name	=> Module_Name,
				Config		=> Load_Configuration(
							Component_Name		=> Component_Name,
							Configuration_Name	=> Module_Name
						),
				Module_ID	=> Module_ID
				);
	end Load_Module;

	------------------------
	-- Service Management --
	------------------------

	function Load_Service(
			Component_Name	: in String;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class is
		-- load a service by it's component name and it's name

		Component : Component_Access := Load( Component_Name );
		Service : Service_Instance_Interface'Class := Create_Instance(
								Component.all,
								Service_Name,
								Service_Mapping
							);
	begin
		Service.Component := Component_Ptr( Component );
		return Service;
	end Load_Service;

	--------------------------------
	-- Component Helper Functions --
	--------------------------------
	function Get_Extension( URI: in String ) return String is
	begin

		for i in reverse URI'Range loop
			if URI( i ) = '.' then
				return URI( i + 1 .. URI'Last );
			end if;
		end loop;

		return "";
	end Get_Extension;
	function Get_Resource( Mapping, URI, Extension: in String ) return String is
		M_Last	: constant integer	:= Mapping'Last;

		function Slashit( Ret: in String ) return String is
		begin
			if Mapping( M_Last ) = '/' then
				return '/' & Ret;	-- does not include the /
			else
				return Ret;		-- does include the /
			end if;
		end Slashit;

	begin
		if Extension = "" then
			declare
				Ret	: constant string	:= URI( URI'First + M_Last + 1 .. URI'Last );
			begin
				return Slashit( Ret );
			end;
		else
			declare
				Ret	: constant string	:= URI( URI'First + M_Last + 1 .. URI'Last - Extension'Length - 1 );
			begin
				return Slashit( Ret );
			end;
		end if;

	end Get_Resource;


	function Locate_Resource(
			Component_Name	: in String;
			Resource	: in String;
			Extension	: in String;
			Kind		: in Ada.Directories.File_Kind	
		) return String is
		-- locate a resource file for this component
		-- this file should be placed at
		-- 	[WORKING_DIR]/data/kowview/component_name/resource.extension
		-- 	or
		-- 	[WORKING_DIR]/applications/component_name/data/resource.extension
		-- returning it's name if nothing has been found raise Ada.Directories.Name_Error if not found
		-- TODO: Implement locale support at Locate_Resource function



		use Ada.Directories;

		MComponent_Name	: String		:= KOW_Lib.String_Util.Str_Replace( From => '.', To => '-', Str => Component_Name );

		Sep		: constant Character	:= KOW_Lib.File_System.Separator;
		Name		: String		:= "data" & Sep & "kowview" & Sep & MComponent_Name & Sep & Resource & "." & Extension;
		Default_Name	: String		:= "applications" & Sep & MComponent_Name & Sep & "data" & Sep & Resource & "." & Extension;






		procedure check( FName : in String ) is
			Real_Kind : File_Kind;
		begin
			Real_Kind := Ada.Directories.Kind( FName );

			if Real_Kind /= Kind then
				raise Ada.Directories.Name_Error with
						"Resource """ & Resource &
						""" of component """ & Component_Name &
						""" is of type """ & File_Kind'Image( Real_Kind ) &
						""" ( expected """ & File_Kind'Image( Kind ) & """)";
			end if;
		exception
			when Ada.IO_Exceptions.Name_Error =>
				raise Ada.Directories.Name_Error with
					"Resource """ & Resource &
					""" of component """ & Component_Name &
					""" ( aka """ & FName & """ ) not found";
		end Check;


	begin

		Check( Name );

		return Name;
	exception
		when others =>
			Check( Default_Name );
			return Default_Name;

	end Locate_Resource;



	function Load_Main_Configuration(
			Component_Name	: in String
		) return KOW_Config.Config_File is
		-- load the main configuration for this component
	begin
		return KOW_Config.New_Config_File(
				N => "kowview" & KOW_Lib.File_System.Separator & Component_Name
			);
	end Load_Main_Configuration;



	function Load_Configuration(
			Component_Name		: in String;
			Configuration_Name	: in String
		) return KOW_Config.Config_File is
		-- load a configuration file from this component's relative path
	begin
		return KOW_Config.New_Config_File(
				N => "kowview" & KOW_Lib.File_System.Separator & Component_Name & KOW_Lib.File_System.Separator & Configuration_Name
			);
	end Load_Configuration;


end KOW_View.Components_Registry;
