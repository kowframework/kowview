------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWView is free software; you can redistribute it  and/or modify it under--
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWView is distributed in the hope that it will be useful,but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWView; see file COPYING.  If not, write--
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
------------------------------------------------------------------------------


---------
-- Ada --
---------
with Ada.Calendar;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with ada.text_io;			use ada.text_io;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;
with KOW_View.Components.Registry;

---------
-- AWS --
---------

with AWS.MIME;
with AWS.Response;
with AWS.Session;
with AWS.Status;
with Templates_Parser;			use Templates_Parser;

package body KOW_View.Themes is
	--------------------
	-- Helper Methods --
	--------------------

	function Locate_Theme_Resource(
			Component_Name	: in String;
			Theme_Name	: in String;
			Resource	: in String;
			Extension	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return KOW_View.Components.Registry.Locate_Resource(
				Component_Name	=> Component_Name,
				Resource	=> Theme_Name & Separator & Resource,
				Extension	=> Extension,
				Kind		=> Kind
			);
	end Locate_Theme_Resource;

	

	function Template_Factory(
			Name	: in String;
			Config	: in KOW_Config.Config_File )
		return Template_Descriptor_Type is
		-- private method for loading the template descriptor from it's configuration
		Descriptor : Template_Descriptor_Type;
	begin
		Descriptor.Name		:= To_Unbounded_String( Name );
		Descriptor.Description	:= KOW_Config.Element( Config, "description" );

		Descriptor.Regions	:= KOW_Lib.String_Util.Explode(
							',',
							To_String(
								KOW_Config.Element( Config, "regions" )
							)
						);
		return Descriptor;
	end Template_Factory;


	
	function Theme_Factory(
			Name	: in String;
			Config	: in KOW_Config.Config_File )
		return Theme_Descriptor_Type is
		-- private method for loading the theme descriptor from it's configuration
		Descriptor : Theme_Descriptor_Type;
	begin
		Descriptor.Name			:= To_Unbounded_String( Name );
		Descriptor.Author		:= KOW_Config.Element( Config, "author" );
		Descriptor.Creation_Date	:= KOW_Config.Element( Config, "creation_time" );

		return Descriptor;
	end Theme_Factory;


	---------------
	-- Component --
	---------------



	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		) is
		-- Initialize the Theme component, setting every variable required
		use KOW_Config;
	begin
		Component.Default_Theme_Name	:= Value( Config, "default_theme", "default" );
		Component.Template_Extension	:= Value( Config, "template_extension", "html" );
		
		Templates_Registry.Factory_Registry.Register(
				"template",
				Template_Factory'Access
			);
		Templates_Registry.Reload_Registry;

		Themes_Registry.Factory_Registry.Register(
				"theme",
				Theme_Factory'Access
			);

		Themes_Registry.Reload_Registry;
	end Initialize;



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return Module_Type'Class is
		-- Creates a module instance
		-- Available modules:
		-- 	template_processor
	begin
		if Module_Name = "template_processor" then
			declare
				Module: Template_Processor_Module;
			begin
				Module.Component_Name		:= To_Unbounded_String( KOW_View.Components.Get_Name( Component ) );
				Module.Default_Theme_Name	:= Component.Default_Theme_Name;
				Module.Template_Extension	:= Component.Template_Extension;
				-- todo: implement loading theme from user's session and profile
				return Module;
			end;
		else
			raise MODULE_ERROR
				with "Module """ &
					Module_Name &
					""" not available in component """ &
					Get_Name( Component ) & """";
		end if;
	end Create_Instance;



	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Type'Class is
		-- create a service.
		-- available services:
		-- 	. theme
	begin
		if Service_Name = "theme" then
			declare
				Service: Theme_Service;
			begin
				Service.Mapping			:= To_Unbounded_String( Service_Mapping );
				Service.Default_Theme_Name	:= Component.Default_Theme_Name;
				Service.Template_Extension	:= Component.Template_Extension;
				return Service;
			end;
		else
			raise SERVICE_ERROR
				with "Service """ &
					Service_Name &
					""" not available in component """ &
					Get_Name( Component ) & """";
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
		Session_ID	: constant AWS.Session.ID := AWS.Status.Session (Request);
		Theme_Name	: constant string := AWS.Session.Get(
							Session_ID,
							theme_name_session_key
						);

	begin
	
	
		if Theme_Name /= "" then
			Module.Theme_Name := To_Unbounded_String( Theme_Name );
		else
			Module.Theme_Name := Module.Default_Theme_Name;
		end if;
		
		Module.Render_Start_Timestamp := Ada.Calendar.Clock;

		Is_Final := false;
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
		use Ada.Calendar;
	begin
		Module.Modules_Finish_Render_Timestamp := Clock;
	end Process_Header;



	procedure Insert_All(
			To	: in out Templates_Parser.Translate_Set;
			Suffix	: in     String;
			Tag_Map	: in out Tag_Maps.Map
		) is
		use Templates_Parser;
	
		procedure Iterator( C: Tag_Maps.Cursor ) is
		begin
			Insert(
				To,
				assoc(
					To_String( Tag_Maps.Key( C ) ) & Suffix,
					Tag_Maps.Element( C )
				)
			);
		end Iterator;
	begin
		Tag_Maps.Iterate( Tag_Map, Iterator'Access );
	end Insert_All;

	overriding
	procedure Process_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- process the request for a module.
		-- sometimes is useful for a module only to be created and released - such as in a page counter module
		use Templates_Parser;
	begin

		Insert( Parameters, assoc( "header", Module.Header_Contents ) );
		Insert( Parameters, assoc( "footer", Module.Footer_Contents ) );

		Insert_All( Parameters, "_header_contents", Module.Module_Header_Contents );
		Insert_All( Parameters, "_contents", Module.Module_Contents );
		Insert_All( Parameters, "_ids", Module.Module_Ids );

		Response := Response & To_Unbounded_String(
					Parse(
						To_String( Module.Template_File_Name ),
						Parameters
					)
				);

	end Process_Request;



	overriding
	procedure Process_Footer(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		use Ada.Calendar;
		Computed_Time : String :=
				Ada.Strings.Fixed.Trim(
					Duration'Image( Clock - Module.Render_Start_Timestamp ),
					Ada.Strings.Both
				);
		Computed_Modules_Time : String :=
				Ada.Strings.Fixed.Trim(
					Duration'Image(
						Module.Modules_Finish_Render_Timestamp - 
						Module.Render_Start_Timestamp
						),
					Ada.Strings.Both
				);
	begin
		Response := Response & To_Unbounded_String(
				"<!-- components rendered in " & Computed_Modules_Time & " secconds // -->"
			);
		Response := Response & To_Unbounded_String(
				"<!-- components + page rendered in " & Computed_Time & " secconds // -->"
			);
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
		Module.Template_Descriptor := Templates_Registry.Registry.Get( Template_Name );
		Module.Template_File_Name := To_Unbounded_String(
			Locate_Theme_Resource(
				Component_Name	=> To_String( Module.Component_Name ),
				Theme_Name	=> To_String( Module.Theme_Name ),
				Resource	=> To_String( Template_Name ),
				Extension	=> To_String( Module.Template_Extension )
			)
		);
	end Set_Template;



	function Get_Regions( Module : in Template_Processor_Module ) return KOW_Lib.UString_Vectors.Vector is
	begin
		return Module.Template_Descriptor.Regions;
	end Get_Regions;



	procedure Generic_Append(
			To_Map		: in out Tag_Maps.Map;
			Region		: in     Unbounded_String;
			Contents	: in     String
		) is
		A_Tag: Tag;
	begin
		if Tag_Maps.Contains( To_Map, Region ) then
			A_Tag := Tag_Maps.Element( To_Map, Region );
		end if;

		A_Tag := A_Tag & Contents;

		Tag_Maps.Include( To_Map, Region, A_Tag );
		-- Notice: include replaces or insert a new element
	end Generic_Append;



	procedure Append_Header(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is

		Str_Contents: String := To_String( Contents );
	begin
		Generic_Append(
			To_Map		=> Module.Module_Header_Contents,
			Region		=> Region,
			Contents	=> Str_Contents
		);

		Module.Header_Contents := Module.Header_Contents & Str_Contents;
	end Append_Header;



	procedure Append_Contents(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is
		use Ada.Strings.Fixed;
	begin
		Generic_Append(
			To_Map		=> Module.Module_Contents,
			Region		=> Region,
			Contents	=> To_String( Contents )
		);

		Generic_Append(
			To_Map		=> Module.Module_Ids,
			Region		=> Region,
			Contents	=> Trim( Integer'Image( Index ), Ada.Strings.Both )
		);
	end Append_Contents;



	procedure Append_Footer(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is
		Str_Contents : String := To_String( Contents );
	begin
		Generic_Append(
			To_Map		=> Module.Module_Footer_Contents,
			Region		=> Region,
			Contents	=> Str_Contents
		);

		Module.Footer_Contents := Module.Footer_Contents & Str_Contents;
	end Append_Footer;


	
	procedure Get_Response(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- Process all the regular module render operations returning the
		-- content rendered.
		--
		-- Notice: Initialize_Request and Finalize_Request should be called elsewhere
	begin
		Process_Header(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters,
			Response	=> Response
		);
		Process_Request(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters,
			Response	=> Response
		);
		Process_Footer(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters,
			Response	=> Response
		);
	end Get_Response;



	--------------
	-- Services --
	--------------



	procedure Process_Request(
			Service		: in out Theme_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is
		-- process request for a theme's static file
		-- only direct access to files that aren't template are alowed
		Session_ID	: constant AWS.Session.ID := AWS.Status.Session (Request);
	
		function Theme_Name return string is
			User_Theme : constant string := AWS.Session.Get( Session_ID, theme_name_session_key );
		begin
			if User_Theme /= "" then
				return User_Theme;
			else
				return To_String( Service.Default_Theme_Name );
			end if;
		end Theme_Name;

		URI		: constant string := AWS.Status.URI( Request );
		Mapping		: constant string := To_String( Service.Mapping );
		Extension	: constant string := KOW_View.Components.Registry.Get_Extension( URI );
		Resource	: constant string := KOW_View.Components.Registry.Get_Resource( Mapping, URI, Extension );
		Component_Name	: constant string := To_String( Service.Component_Name );
		Complete_Path	: constant string := Locate_Theme_Resource(
				Component_Name	=> Component_Name,
				Theme_Name	=> Theme_Name,
				Resource	=> Resource,
				Extension	=> Extension,
				Kind		=> Ada.Directories.Ordinary_File
			);
	begin

		if Extension = To_String( Service.Template_Extension ) then
			raise CONSTRAINT_ERROR with "I can't show you my template sources! Sorry!";
		end if;
		-- if it got here, everything went well
		Response := AWS.Response.File(
				Content_Type	=> AWS.MIME.Content_Type( Complete_Path ),
				Filename	=> Complete_Path
			);
	end Process_Request;



end KOW_View.Themes;
