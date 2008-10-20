

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


	-- TODO: page module
	overriding
	procedure Process_Request(
			Module		: in out Page_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- This is the only procedure implemented by the page module.
		-- That's how it's done so there is no need to deal with dynamic allocation
	begin
		null;
	end Process_Request;

	--
	-- Static Module
	--

	
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
