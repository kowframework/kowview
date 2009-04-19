

---------
-- Ada --
---------

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Text_IO;			use Ada.Text_IO;

---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_Lib.File_System;
with Aw_Lib.String_Util;
with Aw_Lib.UString_Vectors;
with Aw_View.Components;		use Aw_View.Components;
with Aw_View.Components_Registry;
with Aw_View.Themes;			use Aw_View.Themes;

---------
-- AWS --
---------

with AWS.MIME;
with AWS.Response;
with AWS.Status;
with Templates_Parser;



package body Aw_View.Pages is


	-------------------------
	-- Auxiliary Functions --
	-------------------------

	function Load_Page_Config( Config_Name : in String ) return Aw_Config.Config_File is
		-- load a configuration from the page. ;)
	begin
		return Aw_View.Components_Registry.Load_Configuration(
				"pages",
				"page" &
					Aw_Lib.File_System.Separator &
					Config_Name
			);

		-- TODO AQUI
	end Load_Page_Config;




	----------------
	-- Components --
	----------------
	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		) is
		-- the only thing to setup is the theme_component
	begin
		Component.Theme_Component_Name := Aw_Config.Value( Config, "theme_component", "themes" );
	end Initialize;

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class is
		-- Available modules:
		-- 	. page



		function Merge_Page_Parents( Cfg : in Aw_Config.Config_File ) return Aw_Config.Config_File is
			Parent_Cfg	: Aw_Config.Config_File;
			Extends		: String := Aw_Config.Value( Cfg, "extends", "" );
		begin
			if Extends /= "" then
				Parent_Cfg := Load_Page_Config( Extends );
				return Aw_Config.Merge_Configs(
						Parent	=> Merge_Page_Parents( Parent_Cfg ),
						Child	=> Cfg
					);
			else
				return Cfg;
			end if;
		exception
			when e : PAGE_CONFIG_ERROR =>
				Ada.Exceptions.Reraise_Occurrence( E );
			when others => 
				raise PAGE_CONFIG_ERROR with "Error while extending """ & Extends & """";
		end Merge_Page_Parents;

	begin
		if Module_Name = "page" then
			declare
				Module: Page_Module;
			begin
				Module.Config := Merge_Page_Parents( Config );
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
	procedure Initialize_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		) is
	-- this is where the page is initialized.

		use Aw_Config;
		use Aw_View.Components_Registry;

		type Regions_Array is Array( Integer range<> ) of Unbounded_String;
		type Values_Array is Array( Integer range<> ) of Unbounded_String;

		Template_Name		: constant Unbounded_String
						:= Value( Module.Config, "template", "default" );
		Modules_Cfg		: constant Config_File_Array
						:= Elements_Array( Module.Config, "modules" );
		Theme_Component_Name	: constant String := To_String( Module.Theme_Component_Name );
		Available_Regions	: Aw_Lib.UString_Vectors.Vector;

		Module_Regions		: Regions_Array( Modules_Cfg'Range );
		-- map each module to it's region
		Headers			: Values_Array( Modules_Cfg'Range );
		Contents		: Values_array( Modules_Cfg'Range );
		Footers			: Values_Array( Modules_Cfg'Range );

		procedure Region_Iterator( C: in Aw_Lib.UString_Vectors.Cursor ) is
			-- assemble the vector containing the modules to render.
			use Aw_Lib.UString_Vectors;

			Modules_Str: String := Aw_Config.Element( Module.Config, To_String( Element( C ) ) );
			Modules: Vector := Aw_Lib.String_Util.Explode( ',', Modules_Str );
			

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

		procedure Region_Append_Iterator( C: in Aw_Lib.UString_Vectors.Cursor ) is
			-- assemble the vector containing the modules to render.
			use Aw_Lib.UString_Vectors;

			Current_Region : constant Unbounded_String := Element( C );

			Modules_Str: String := Aw_Config.Element( Module.Config, To_String( Current_Region ) );
			Modules: Vector := Aw_Lib.String_Util.Explode( ',', Modules_Str );
			

			procedure Module_Iterator( C2: in Cursor ) is
				i: Integer;
			begin
				i := Integer'Value( To_String( Element( C2 ) ) );
				
				Append_Header(
					Module.Processor, Current_Region, i, Headers( i )
				);
				Append_Contents(
					Module.Processor, Current_Region, i, Contents( i )
				);
				Append_Footer(
					Module.Processor, Current_Region, i, Footers( i )
				);

			end Module_Iterator;
		begin

			Iterate( Modules, Module_Iterator'Access );
		end Region_Append_Iterator;


	begin
		Module.Processor := Aw_View.Themes.Template_Processor_Module(
					Aw_View.Components_Registry.Load_Module(
						Component_Name	=> Theme_Component_Name,
						Module_Name	=> "template_processor"
					)
				);

		Aw_View.Themes.Initialize_Request(
			Module		=> Module.Processor,
			Request		=> Request,
			Parameters	=> Parameters,
			Response	=> Response,
			Is_Final	=> Is_Final
		);

		Set_Template( Module.Processor, Template_Name );

		Available_Regions := Get_Regions( Module.Processor );
		-- get all available regions in the template
	

		Aw_Config.Set_Section( Module.Config, "positions" );
		Aw_Lib.UString_Vectors.Iterate( Available_Regions, Region_Iterator'Access );
		-- now we setup the regions for each module.
		-- each module can appear only once.


		if Is_Final then
			return;
		end if;

		for i in Modules_Cfg'Range loop
			declare
				Cfg		: Config_File := Modules_Cfg( i );
				Inner_Module	: Module_Instance_Interface'Class
							:= Load_Module(
								To_String( Element( cfg, "component" ) ),
								To_String( Element( cfg, "module" ) ),
								Cfg
							);
			begin
				Initialize_Request(
					Module		=> Inner_Module,
					Request		=> Request,
					Parameters	=> Parameters,
					Response	=> Response,
					Is_Final	=> Is_Final
				);

				if Is_Final then
					-- if it's final, nothing else should be processed
					return;
				end if;

				if Module_Regions( i ) /= Null_Unbounded_String then

					Process_Header(
						Module		=> Inner_Module,
						Request		=> Request,
						Parameters	=> Parameters,
						Response	=> Headers( i )
					);
	
					Process_Request(
						Module		=> Inner_Module,
						Request		=> Request,
						Parameters	=> Parameters,
						Response	=> Contents( i )
					);
					Process_Footer(
						Module		=> Inner_Module,
						Request		=> Request,
						Parameters	=> Parameters,
						Response	=> Footers( i )
					);


				end if;
	
				Finalize_Request(
					Module		=> Inner_Module,
					Request		=> Request,
					Parameters	=> Parameters
				);


			end;
		end loop;
		-- now we append the values in the right order..
		Aw_Config.Set_Section( Module.Config, "positions" );
		Aw_Lib.UString_Vectors.Iterate( Available_Regions, Region_Append_Iterator'Access );



	end Initialize_Request;

	overriding
	procedure Process_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- it's where the page is assembled.
	begin
		Get_Response(
				Module		=> Module.Processor,
				Request		=> Request,
				Parameters	=> Parameters,
				Response	=> Response
			);
	end Process_Request;
	

	overriding
	procedure Finalize_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
		) is
	begin
		if Module.Processor not in Aw_View.Themes.Template_Processor_Module then
			return;
			-- this is to avoid GNAT warnings.
		end if;
		Finalize_Request(
			Module		=> Module.Processor,
			Request		=> Request,
			Parameters	=> Parameters
			);
	end Finalize_Request;



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
		Parameters : Templates_Parser.Translate_Set;
		-- a null set
		
		use Aw_View.Components_Registry;

		Module		: Module_Instance_Interface'Class :=
					Aw_View.Components_Registry.Load_Module(
							Component_Name	=> "pages",
							Module_Name	=> "page",
							Config		=> Load_Page_Config(
											Get_Resource(
												To_String( Service.Mapping ),
												AWS.Status.URI( Request ),
												""
											)
									)
						);

		Text_Output	: Unbounded_String;
		Is_Final	: Boolean;
	begin
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

		Process_Request(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters,
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
		use Aw_View.Components_Registry;
		
		URI	: constant string := AWS.Status.URI( Request );
		Mapping	: constant string := To_String( Service.Mapping );

		Extension	: constant string := Get_Extension( URI );
		Resource	: constant string := Get_Resource( Mapping, URI, Extension );

		Path		: Unbounded_String;


		Prefix : constant String := To_String( Service.Mapping ) & Aw_Lib.File_System.Separator;


	begin
		begin
			Path := To_Unbounded_String(
					Aw_View.Components_Registry.Locate_Resource(
							Component_Name		=> "pages",
							Resource		=> Prefix & Resource,
							Extension		=> Extension,
							Kind			=> Ada.Directories.Ordinary_File
					)
				);
		exception
			when Ada.Directories.Name_Error =>
				-- look for a index file
				Path := To_Unbounded_String(
						Aw_View.Components_Registry.Locate_Resource(
								Component_Name		=> "pages",
								Resource		=> Prefix & Resource & "." & Extension & Aw_Lib.FIle_System.Separator & "index",
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
