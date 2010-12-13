------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------


---------
-- Ada --
---------
with Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Generic_Registry;
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;		use KOW_View.Components;

---------
-- AWS --
---------

with AWS.Response;
with AWS.Status;
with Templates_Parser;

-- TODO: create module for theme listing
-- TODO: create service for theme selection

package KOW_View.Themes is

	--------------------
	-- Helper Methods --
	--------------------

	theme_name_session_key: constant string := "KOW_view::theme_name";

	function Locate_Theme_Resource(
			Component_Name	: in String;
			Theme_Name	: in String;
			Resource	: in String;
			Extension	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String;

	---------------
	-- Component --
	---------------


	type Component_Type is new KOW_View.Components.Component_Type with private;

	

	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		);
	-- Initialize the Theme component, setting every variable required


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return Module_Type'Class;
	-- Creates a module instance
	-- Available modules:
	-- 	template_processor


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Type'Class;
	-- create a service.
	-- available services:
	-- 	. theme

	-------------
	-- Modules --
	-------------


	type Template_Processor_Module is new Module_Type with private;
	-- The template module represents any template
	-- All it does is to load a string representing the AWS template as string
	-- and provide methods for pa
	--
	-- This is how theme engines are implemented in KOW_View. You can also implement your own
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

	--------------
	-- Services --
	--------------


	type Theme_Service is new Service_Type with private;
	-- Map a URI to theme resources

	overriding
	procedure Process_Request(
			Service		: in out Theme_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	-- process request for a theme's static file
	-- only direct access to files that aren't template are alowed




	----------------------------------------
	-- Theme Management and configuration --
	----------------------------------------

	-- The following methods and types are for internal use only.
	--
	-- That's what runs KOW_View.Themes component.


	type Theme_Descriptor_Type is record
		-- A record type for describing how a theme operates and other information.
		Name		: Unbounded_String;
		Author		: Unbounded_String;
		Creation_Date	: Unbounded_String; -- TODO: store the creation date as Ada.Calendar.Time
	end record;
	

	type Template_Descriptor_Type is record
		-- A record type describing a template.
		-- This is used to describe the templates that are expected to be found in
		-- all themes.
		--
		-- Also, contains information about the author and why is this template required.
		Name		: Unbounded_String;
		Description	: Unbounded_String;
		Regions		: KOW_Lib.UString_Vectors.Vector;
	end record;

	package Themes_Registry is new KOW_Config.Generic_Registry(
				Element_Type	=> Theme_Descriptor_Type,
				Relative_Path	=> "kowview" & Separator & "themes" & Separator & "themes"
			);
	-- Store all the available theme's descriptor.


	package Templates_Registry is new KOW_Config.Generic_Registry(
				Element_Type	=> Template_Descriptor_Type,
				Relative_Path	=> "kowview" & Separator & "themes" & Separator & "templates"
			);
	-- Store all required templates.


private


	type Component_Type is new KOW_View.Components.Component_Type with record
		Default_Theme_Name	: Unbounded_String; -- default
		Name			: Unbounded_String;
		Template_Extension	: Unbounded_String; -- html
	end record;
	


	package Tag_Maps is new Ada.Containers.Ordered_Maps(
					Key_Type	=> Unbounded_String,
					Element_Type	=> Templates_Parser.Tag,
					"="		=> Templates_Parser."="
				);

	type Template_Processor_Module is new Module_Type with record
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

	type Theme_Service is new Service_Type with record
		Mapping			: Unbounded_String;
		Component_Name		: Unbounded_String;
		Default_Theme_Name	: Unbounded_String;
		Template_Extension	: Unbounded_String;
	end record;
end KOW_View.Themes;
