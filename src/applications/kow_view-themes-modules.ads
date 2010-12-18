	-------------
	-- Modules --
	-------------


	type Template_Processor_Module is new Module_Type with record
		-- The template module represents any template
		-- All it does is to load a string representing the AWS template as string
		-- and provide methods for pa
		--
		-- This is how theme engines are implemented in KOW_View. You can also implement your own
		-- (this requires changing the Create_Instance return Module_Instance_Iterface'Class method)


		Component_name		: Unbounded_String;
		Default_Theme_Name	: Unbounded_String;
		Theme_Name		: Unbounded_String;
		Template_Descriptor	: Template_Descriptor_Type;
		Template_File_Name	: Unbounded_String;
		Template_Extension	: Unbounded_String;

		Render_Start_Timestamp		: Ada.Calendar.Time;
		Modules_Finish_Render_Timestamp	: Ada.Calendar.Time;

		Header_Contents			: Templates_Parser.Tag;
		Module_Header_Contents		: Tag_Maps.Map;
		-- the user can access every header or only one of the headers;

		Module_Contents			: Tag_Maps.Map;
		Module_Ids			: Tag_Maps.Map;
		-- thats where the module contents are stored
	
		Module_Footer_Contents		: Tag_Maps.Map;
		Footer_Contents			: Templates_Parser.Tag;
		-- the same for the header is for the footer
	end record;




	overriding
	procedure Initialize_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Config		: in out KOW_Config.Config_File
		);
	
	overriding
	procedure Process_Header(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Response	: in out Unbounded_String
		);
	-- process header of the response.
	-- it's assumed that 

	overriding
	procedure Process_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Response	: in out Unbounded_String
		);
	-- process the request for a module.
	-- sometimes is useful for a module only to be created and released - such as in a page counter module

	overriding
	procedure Process_Footer(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Response	: in out Unbounded_String
		);
	-- process some footer of the module
	-- useful when creating benchmar modules

	overriding
	procedure Finalize_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data
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

	function Get_Regions( Module : in Template_Processor_Module ) return KOW_Lib.UString_Vectors.Vector;

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
	
	procedure Get_Response(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- Process all the regular module render operations returning the
	-- content rendered.
	--
	-- Notice: Initialize_Request and Finalize_Request should be called elsewhere


