

---------
-- Ada --
---------
with Ada.Calendar;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_Lib.File_System;		use Aw_Lib.File_System;
with Aw_Lib.UString_Vectors;
with Aw_View.Components;

---------
-- AWS --
---------

with AWS.Response;
with AWS.Status;
with Templates_Parser;

package body Aw_View.Themes is



	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		) is
		-- Initialize the Theme component, setting every variable required
		use Aw_Config;
	begin
		Component.Default_Theme := Value( Config, "default_theme", "default" );
		Component.Name := To_Unbounded_String( Component_Name );

	end Initialize;



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class is
		-- Creates a module instance
		-- Available modules:
		-- 	template_processor
	begin
		if Module_Name = "template_processor" then
			declare
				Module: Template_Processor_Module;
			begin
				return Module;
			end;
		else
			raise MODULE_ERROR
				with "Module """ &
					Module_Name &
					""" not available in component """ &
					To_String( Component.Name ) & """";
		end if;
	end Create_Instance;



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class is
		-- create a service.
		-- available services:
		-- 	. theme
	begin
		if Service_Name = "theme" then
			declare
				Service: Theme_Service;
			begin
				Service.Mapping := To_Unbounded_String( Service_Mapping );
				return Service;
			end;
		else
			raise SERVICE_ERROR
				with "Service """ &
					Service_Name &
					""" not available in component """ &
					To_String( Component.Name ) & """";
		end if;
	end Create_Instance;



	-------------
	-- Modules --
	-------------



	overriding
	procedure Initialize_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		) is
	begin
		Is_Final := false;
		null;
	end Initialize_Request;
	


	overriding
	procedure Process_Header(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- process header of the response.
		-- it's assumed that 
	begin
		null;
	end Process_Header;



	overriding
	procedure Process_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- process the request for a module.
		-- sometimes is useful for a module only to be created and released - such as in a page counter module
	begin
		null;
	end Process_Request;



	overriding
	procedure Process_Footer(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- process some footer of the module
		-- useful when creating benchmar modules
	begin
		null;
	end Process_Footer;



	overriding
	procedure Finalize_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
		) is
		-- Finalize processing the request.
		-- Called when the process has been finalized
	begin
		null;
	end Finalize_Request;



	procedure Set_Template(
			Module		: in out Template_Processor_Module;
			Template_Name	: in     Unbounded_String
		) is
		-- Load a template configuration and prepare for processing
	begin
		null;
	end Set_Template;



	function Get_Regions( Module : in Template_Processor_Module ) return Aw_Lib.UString_Vectors.Vector is
	begin
		return Module.Descriptor.Regions;
	end Get_Regions;



	procedure Append_Header(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is
	begin
		null;
	end Append_Header;



	procedure Append_Contents(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is
	begin
		null;
	end Append_Contents;



	procedure Append_Footer(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is
	begin
		null;
	end Append_Footer;


	
	function Get_Response(
			Module		: in Template_Processor_Module;
			Request		: in AWS.Status.Data;
			Parameters	: in Templates_Parser.Translate_Set
		) return Unbounded_String is
		-- Process all the regular module render operations returning the
		-- content rendered.
		--
		-- Notice: Initialize_Request and Finalize_Request should be called elsewhere
		Response: Unbounded_String;
	begin
		return Response;
	end Get_Response;



	--------------
	-- Services --
	--------------



	procedure Process_Request(
			Service		: in out Theme_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is
		-- process a request to a service
		-- the entire request is handled by the service
		-- sometimes is useful for a service only to be created and released - such as in a counter service
	begin
		null;
	end Process_Request;

end Aw_View.Themes;
