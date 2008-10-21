

---------
-- Ada --
---------

with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_Lib.File_System;
with Aw_View.Components;		use Aw_View.Components;
with Aw_View.Components_Registry;

---------
-- AWS --
---------

with AWS.MIME;
with AWS.Response;
with AWS.Status;
with Templates_Parser;



package body Aw_View.Pages is


	----------------
	-- Components --
	----------------

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class is
		-- Available modules:
		-- 	. page
	begin
		if Module_Name = "page" then
			declare
				Module: Page_Module;
			begin
				Module.Config := Config;
				Module.Theme_Component_Name := Component.Theme_Component_Name;
				return Module;
			end;
		else
			raise Module_Error with "Module """ & Module_Name & """ doesn't exist";
		end if;
	end Create_Instance;



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class is
		-- Available services:
		-- 	. page
		-- 	. static
	begin

		if Service_Name = "page" then
			declare
				Service: Page_Service;
			begin
				Service.Mapping := To_Unbounded_String( Service_Mapping );
				return Service;
			end;
		elsif Service_Name = "static" then
			declare
				Service: Static_Service;
			begin
				Service.Mapping := To_Unbounded_String( Service_Mapping );
				return Service;
			end;
		else
			raise SERVICE_ERROR with "Service """ & Service_Name & """ doesn't exist";
		end if;
	end Create_Instance;


	-------------
	-- Modules --
	-------------



	--
	-- Page Module
	--


	overriding
	procedure Process_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- This is the only procedure implemented by the page module.
		-- That's how it's done so there is no need to deal with dynamic allocation

		use Aw_Config;
		use Aw_View.Components_Registry;

		type Regions_Array is Array( Integer range<> ) of Unbounded_String;

		Template_Name	: constant Unbounded_String
					:= Value( Module.Config, "template", "default" );
		Modules_Cfg	: constant Config_File_Array
					:= Elements_Array( Module.Config, "modules" );

		Theme_Component_Name	: constant String := To_String( Module.Theme_Component_Name );


		function Load_Processor return Aw_View.Themes.Template_Processor_Module'Class is
			pragma Inline( Load_Processor );

			-- notice: it's here only to clean up the syntax
		begin
			return Aw_View.Themes.Template_Processor_Module'Class(
					Load_Module(
						Component	=> Theme_Component_Name,
						Module		=> "template_processor"
					);
		end Load_Processor;

		Processor		: Aw_View.Themes.Template_Processor_Module'Class := Load_Processor;
		Is_Final		: Boolean;
		Available_Regions	: Aw_Lib.UString_Vectors.Vector;

		Module_Regions		: Regions_Array( Modules_Cfg'Range );
		-- map each module to it's region


		procedure Region_Iterator( C: in Aw_Lib.UString_Vectors.Cursor ) is
			-- assemble the vector containing the modules to render.
			use Aw_Lib.UString_Vectors;

			Modules: Vector := Aw_Lib.String_Util.Explode( ',' Element( C ) );

			procedure Module_Iterator( C2: in Cursor ) is
				index: Integer;
			begin
				index := Integer'Value( To_String( Element( C2 ) ) );

				if Module_Regions( index ) /= Null_Unbounded_String then
					raise MODULE_ERROR with "Region for module """ & To_String( Element( C2 ) ) & """ already set";
				end if;
				Module_Regions( index ) := Element( C );
			exception
				when CONSTRAINT_ERROR =>
					raise MODULE_ERROR with "Impossible to assemble page using module """ & To_String( Element( C2 ) ) & """";
			end Module_Iterator;
		begin

			Iterate( Modules, Module_Iterator'Access );
		end Region_Iterator;


	begin
		Set_Template( Processor, Template_Name );

		Available_Regions := Get_Regions( Processor );
		-- get all available regions in the template
	

		Aw_Lib.UString_Vectors.Iterate( Available_Regions, Region_Iterator'Access );
		-- now we setup the regions for each module.
		-- each module can appear only once.

		Initialize_Request(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters,
			Response	=> Response,
			Is_Final	=> Is_Final
		);

		if Is_Final then
			return;
		end if;

		for i in Modules_Cfg'Range loop
			declare
				Cfg		: Config_File := Modules_Cfg( i );
				Module		: Module_Instance_Interface'Class
							:= Load_Module(
								Element( cfg, "component" ),
								Element( cfg, "module" ),
								Cfg
							);
				Header		: Unbounded_String;
				Contents	: Unbounded_String;
				Footer		: Unbounded_String;
				
				use Aw_Lib.UString_Vectors;
				procedure Region_Iterator( C: in Cursor ) is
				begin
					Append_Header(
						Module		=> Processor,
						Region		=> Element( C ),
						Component_Id	=>

			begin
				Initialize_Request(
					Module		=> Module,
					Request		=> Request,
					Parameters	=> Parameters,
					Response	=> Response,
					Is_Final	=> Is_Final
				);

				if Is_Final then
					-- if it's final, nothing else should be processed
					return;
				end if;

				if Should_Draw( i ) then
					Process_Header(
						Module		=> Module,
						Request		=> Request,
						Parameters	=> Parameters,
						Response	=> Header
					);
	
					Process_Request(
						Module		=> Module,
						Request		=> Request,
						Parameters	=> Parameters,
						Response	=> Contents
					);
					Process_Footer(
						Module		=> Module,
						Request		=> Request,
						Parameters	=> Parameters,
						Response	=> Footer
					);


					Append_Header(	
						Processor, Module_Regions( i ), i, Header );
					Append_Contents(
						Processor, Module_Regions( i ), i, Contents );
					Append_Footer(
						Processor, Module_Regions( i ), i, Footer );
				end if;
	
				Finalize_Request(
					Module		=> Module,
					Request		=> Request,
					Parameters	=> Parameters
				);


			end;
		end loop;

		Response := Get_Response(
				Module		=> Processor
				Request		=> Request,
				Parameters	=> Parameters
			);


	end Process_Request;

	
	--------------
	-- Services --
	-------------


	--
	-- Page Service
	--


	overriding
	procedure Process_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is

		-- This service provides direct access to the page module.
		Translate_Set	: Templates_Parser.Translate_Set;
		-- a null set
		
		use Aw_View.Components_Registry;

		Module		: Module_Instance_Interface'Class :=
					Aw_View.Components_Registry.Load_Module(
							Component_Name	=> "page",
							Module_Name	=> "page",
							Config		=> Load_Configuration( "page", AWS.Status.URI( Request ) )
						);

		Text_Output: Unbounded_String;
	begin

		Process_Request(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Translate_Set,
			Response	=> Text_Output
		);

		Response := AWS.Response.Build (
				Content_Type    => "text/html",
				Message_Body    => To_String( Text_Output )
			);

		Finalize_Request(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters
		);

	end Process_Request;

	--
	-- Static Service
	-- 

	
	overriding
	procedure Process_Request(
			Service		: in out Static_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is
		-- This service acts like a standard web server, providing access
		-- to static files.

		
		URI	: constant string := AWS.Status.URI( Request );
		Mapping	: constant string := To_String( Service.Mapping );

		function Get_Extension return String is
		begin

			for i in reverse URI'Range loop
				if URI( i ) = '.' then
					return URI( i + 1 .. URI'Last );
				end if;
			end loop;

			return "";
		end Get_Extension;
		Extension	: constant string := Get_Extension;

		function Get_Resource return String is
			M_Last	: constant integer	:= Mapping'Last;
			Ret	: constant string	:= URI( URI'First + M_Last + 1 .. URI'Last - Extension'Length - 1 );
		begin

			if Mapping( M_Last ) = '/' then
				return '/' & Ret;	-- does not include the /
			else
				return Ret;		-- does include the /
			end if;
		end Get_Resource;
		
		Resource	: constant string := Get_Resource;
		Path		: Unbounded_String;


	begin
		begin
			Path := To_Unbounded_String(
					Aw_View.Components_Registry.Locate_Resource(
							Component_Name		=> "page",
							Resource		=> Resource,
							Extension		=> Extension,
							Kind			=> Ada.Directories.Ordinary_File
					)
				);
		exception
			when Ada.Directories.Name_Error =>
				-- look for a index file
				Path := To_Unbounded_String(
						Aw_View.Components_Registry.Locate_Resource(
								Component_Name		=> "page",
								Resource		=> Resource & "." & Extension & Aw_Lib.FIle_System.Separator & "index",
								Extension		=> "html",
								Kind			=> Ada.Directories.Ordinary_File
						)
				);
		end;

		-- if got here, I found at least the index file


		declare	
			Str_Path: constant String := To_String( Path );
		begin
			Response := AWS.Response.File (
				Content_Type    => AWS.MIME.Content_Type( Str_Path ),
				Filename        => Str_Path
			);
		end;

	end Process_Request;


end Aw_View.Pages;
