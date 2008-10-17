

---------
-- Ada --
---------
with Ada.IO_Exceptions;
with Ada.Directories;			use Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_Config.Text;
with Aw_Config.Generic_Registry;
with Aw_Lib.File_System;
with Aw_Lib.UString_Vectors;
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
			Config := Load_Main_Configuration( Component_Name );
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

	--------------------------------
	-- Component Helper Functions --
	--------------------------------

	function Locate_Resource(
			Component_Name	: in String;
			Resource	: in String;
			Kind		: in Ada.Directories.File_Kind	
		) return String is
		-- locate a resource file for this component in the Aw_Config's configuration path
		-- returning it's name if nothing has been found raise Ada.Direct_IO.Name_Error if not found

		use Aw_Lib.UString_Vectors;

		Config_Path: Aw_Lib.UString_Vectors.Vector := Aw_Config.Get_Config_Path;

		Path: Unbounded_String := Null_Unbounded_String;

		Filter: Filter_Type := ( others => false );


		Relative_Path: String := Component_Name & "/" & Resource;


		function Get_Resource_Name return String is
		begin
			for i in reverse Resource'Range loop
				if Resource( i ) = Aw_Lib.File_System.Separator and i /= Resource'Last then
					return Resource( i + 1 .. Resource'Last );
				end if;
			end loop;

			return Resource;
		end Get_Resource_Name;

		Resource_Name: String := Get_Resource_Name;

		function Get_Resource_Path return String is
		begin
			return Relative_Path( Relative_Path'First .. Relative_Path'Last - Resource_Name'Length );
		end Get_Resource_Path;


	
		Resource_Path: String := Get_Resource_Path;


		procedure Path_Iterator( C: in Aw_Lib.UString_Vectors.Cursor ) is
			-- for each element in the config_path, look for the resource.
			-- once it has been found, set Path and stop searching the directory
			procedure Process_Search( Directory_Entry : Directory_Entry_Type ) is
			begin
				Path := Element( C ) & To_Unbounded_String( Aw_Lib.File_System.Separator &  Relative_Path );
			end Process_Search;

			function Directory return String is
				pragma Inline( Directory );
			begin
				return To_String( Element( C ) ) & Aw_lib.File_System.Separator & Resource_Path;

			end Directory;
		begin
			if Path /= Null_Unbounded_String then
				-- it means we've found the path!
				return;
			end if;
			
			Search( Directory       => Directory,
				Pattern         => Resource_Name,
				Filter          => Filter,
				Process         => Process_Search'Access );
		exception
			when ADA.IO_EXCEPTIONS.NAME_ERROR => null;
		end Path_Iterator;
	begin
		Aw_Lib.Ustring_Vectors.Iterate( Config_Path, Path_Iterator'Access );

		Filter( Kind ) := True;

		if Path = Null_Unbounded_String then
			raise NAME_ERROR with "Impossible to load resource """ & Resource & """ for component """ & Component_Name & """";
		end if;

		return To_String( Path );

	end Locate_Resource;



	function Load_Main_Configuration(
			Component_Name	: in String
		) return Aw_Config.Config_File is
		-- load the main configuration for this component
	begin
		return Aw_Config.New_Config_File(
				N => "awconfig" & Aw_Lib.File_System.Separator & Component_Name,
				P => new Aw_Config.Text.Parser -- todo: change this to a more sane approach
			);
	end Load_Main_Configuration;



	function Load_Configuration(
			Component_Name		: in String;
			Configuration_Name	: in String
		) return Aw_Config.Config_File is
		-- load a configuration file from this component's relative path
	begin
		return Aw_Config.New_Config_File(
				N => "awconfig" & Aw_Lib.File_System.Separator & Component_Name & Aw_Lib.File_System.Separator & Configuration_Name,
				P => new Aw_Config.Text.Parser -- todo: change this to a more sane approach
			);
	end Load_Configuration;


end Aw_View.Component_Registry;
