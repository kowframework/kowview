

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

-- TODO: create module for theme listing
-- TODO: create service for theme selection

package Aw_View.Themes is


	type Component_Type is new Aw_View.Components.Component_Interface with private;


	

	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		);
	-- Initialize the Theme component, setting every variable required


	overriding
	function Create_Instance(
			Component	: in Template_Assembler_Module;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class;
	-- Creates a module instance
	-- Available modules:
	-- 	template_processor


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class is abstract;
	-- create a service.
	-- available services:
	-- 	. theme

	-------------
	-- Modules --
	-------------


	type Template_Processor_Module is new Module_Instance_Interface with private;
	-- The template module represents any template
	-- All it does is to load a string representing the AWS template as string
	-- and provide methods for pa
	--
	-- This is how theme engines are implemented in Aw_View. You can also implement your own
	-- (this requires changing the Create_Instance return Module_Instance_Iterface'Class method)




	overriding
	procedure Initialize_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		);
	
	overriding
	procedure Process_Header(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- process header of the response.
	-- it's assumed that 

	overriding
	procedure Process_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- process the request for a module.
	-- sometimes is useful for a module only to be created and released - such as in a page counter module

	overriding
	procedure Process_Footer(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- process some footer of the module
	-- useful when creating benchmar modules

	overriding
	procedure Finalize_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
		);
	-- Finalize processing the request.
	-- Called when the process has been finalized

	--
	-- new methods
	--
	-- These methods are the ones called by the page component.
	-- Instead of calling the traditional modules, we implemented other ones more meaninfull for
	-- what we are doing. The standard methods are implemented for this engine.
	--
	-- It might be suficient to override them, but you can also override those methods:
	
	-- How things work?
	--
	-- The first thing that happens it the call for Set_Template. This procedure load the template
	-- configuration for the current theme, including all the available regions.
	--
	-- Then Initialize_Request is called - which should be used to setup the initial parameters and
	-- the AWS template parser.
	--
	-- The next step is to process modules.
	--
	-- When processing modules, the page component call these functions (in order for each module):
	-- 	. Append_Header
	-- 	. Append_Contents
	-- 	. Append_Footer
	-- Then, at the end of processing, it calls Get_Response which is responsible for assembling
	-- everything.

	procedure Set_Template(
			Module		: in out Template_Processor_Module;
			Template_Name	: in     Unbounded_String
		);
	-- Load a template configuration and prepare for processing

	function Get_Regions( Module : in Templante_Processor_Module ) return Aw_Lib.UString_Vectors.Vector;

	procedure Append_Header(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		);

	procedure Append_Contents(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		);


	procedure Append_Footer(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		);
	
	function Get_Response(
			Module		: in Template_Processor_Module;
			Region		: in     Unbounded_String;
			Request		: in AWS.Status.Data;
			Parameters	: in Templates_Parser.Translate_Set
		) return Unbounded_String;
	-- Process all the regular module render operations returning the
	-- content rendered.
	--
	-- Notice: Initialize_Request and Finalize_Request should be called elsewhere

	--------------
	-- Services --
	--------------


	type Theme_Service is new Theme_Instance_Interface with private;
	-- Map a URI to theme resources


	procedure Process_Request(
			Service		: in out Service_Instance_Interface;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- process a request to a service
	-- the entire request is handled by the service
	-- sometimes is useful for a service only to be created and released - such as in a counter service




	----------------------------------------
	-- Theme Management and configuration --
	----------------------------------------

	-- The following methods and types are for internal use only.
	--
	-- That's what runs Aw_View.Themes component.


	type Theme_Descriptor_Type is record
		-- A record type for describing how a theme operates and other information.
		Name		: Unbounded_String;
		Author		: Unbounded_String;
		Creation_Date	: Ada.Calendar.Time;
	end record;
	

	type Template_Descriptor_Type is record
		-- A record type describing a template.
		-- This is used to describe the templates that are expected to be found in
		-- all themes.
		--
		-- Also, contains information about the author and why is this template required.
		Name		: Unbounded_String;
		Description	: Unbounded_String;
		Regions		: Aw_Lib.UString_Vectors.Vector;
	end record;

	package Themes_Registry is new Aw_Config.Generic_Registry(
				Element_Type	=> Theme_Descriptor_Type,
				Relative_Path	=> "awview" & Separator & "themes" & Separator & "themes"
			);
	-- Store all the available theme's descriptor.


	package Templates_Registry is new Aw_Config.Generic_Registry(
				Element_Type	=> Template_Descriptor_Type,
				Relative_Path	=> "awview" & Separator & "themes" & Separator & "templates"
			);
	-- Store all required templates.


private


	type Component_Type is new Aw_View.Components.Component_Interface with record
		Default_Theme	: Unbounded_String;
	end record;
	
	type Template_Processor_Module is new Module_Instance_Interface with record
		Template_Name		: Unbounded_String;
		Template_Descriptor	: Unbounded_String;
		Template_File		: Unbounded_String;
		Descriptor		: Template_Descriptor_Type;
	end record;

	type Theme_Service is new Theme_Instance_Interface with record
		Mapping		: Unbounded_String;
	end record;
end Aw_View.Themes;
