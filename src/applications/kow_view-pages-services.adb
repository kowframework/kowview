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
pragma License( GPL );


------------------------------------------------------------------------------
-- Main package for the Pages services                                      --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.File_System;
with KOW_Lib.Json;
with KOW_View.Components;				use KOW_View.Components;
with KOW_View.Components.Util;
with KOW_View.Pages.Components;
with KOW_View.Modules;					use KOW_View.Modules;
with KOW_View.Services;
with KOW_View.Services.Stateless_Service_Cycles;
with KOW_View.Services.Util;
with KOW_View.Themes;
with KOW_View.Themes.Template_Processors;


---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;


package body KOW_View.Pages.Services is



	--------------------
	-- Helper Methods --
	--------------------
	procedure Iterate(
				Modules		: in out Complete_Module_Array;
				Iterator	: access procedure( Module : in out Complete_Module_Type );
				Require_Region	: Boolean 
			) is
		use KOW_View.Themes;
	begin
		for i in Modules'Range loop 
			if not Require_Region OR ELSE Modules(i).Region /= "" then
				Iterator.all( Modules( i ) );
			end if;
		end loop;
	end Iterate;

	----------------------
	-- The Page Service --
	----------------------


	overriding
	procedure Process_Json_Request(
			Service	: in out Page_Service;
			Request	: in     AWS.Status.Data;
			Response:    out KOW_Lib.Json.Object_Type
		) is
		-- run initialize for each one of the modules in the page
		-- then call Process_Json_Request for a given module or group of modules.

		use KOW_View.Themes.Template_Processors;


		Page		: constant String := Get_Page( Service, Request );
		Config		: KOW_Config.Config_File := Get_Config_File( Page );

		Modules		: Complete_Module_Array := Get_Modules( Config );
		Processor	: Template_Processor_Type := New_Template_Processor( Service.Template );

		Module_Id	: Positive := 1;

		The_Response	: KOW_Lib.Json.Object_Type;

		procedure Create( Complete : in out Complete_Module_Type ) is
		begin
			Create(
					Factory		=> Complete.Factory.all,
					Request		=> Request,
					Context		=> Page,
					Module_ID	=> Module_ID,
					Module		=> Complete.Module
				);
			Module_Id := Module_ID + 1;
		end Create;




		procedure Initialize( Complete : in out Complete_Module_Type ) is
		begin
			Initialize_Request(
					Module	=> Complete.Module.all,
					Request	=> Request,
					Config	=> Complete.Config
				);
		end Initialize;


		procedure Finalize( Complete : in out Complete_Module_Type ) is
		begin
			Finalize_Request(
					Module	=> Complete.Module.all,
					Request	=> Request
				);
		end Finalize;

		procedure Destroy( Complete : in out Complete_Module_Type ) is
		begin
			Destroy(
					Factory		=> Complete.Factory.all,
					Request		=> Request,
					Module		=> Complete.Module
				);
		end Destroy;


	begin
		Iterate( Modules => Modules, Iterator => Create'Access,		Require_Region => False );
		Iterate( Modules => Modules, Iterator => Initialize'Access,	Require_Region => False );

		-- see wich json module should be called :)

		Iterate( Modules => Modules, Iterator => Finalize'Access,	Require_Region => False );
		Iterate( Modules => Modules, Iterator => Destroy'Access,	Require_Region => False );


		Response := The_Response;
	end Process_Json_Request;


	overriding
	procedure Process_Custom_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		) is
		-- process the entire module cycle returning a HTML page

		use KOW_View.Themes.Template_Processors;

		Page		: constant String := Get_Page( Service, Request );
		Config		: KOW_Config.Config_File := Get_Config_File( Page );

		Modules		: Complete_Module_Array := Get_Modules( Config );
		Processor	: Template_Processor_Type := New_Template_Processor( Service.Template );

		Module_Id	: Positive := 1;


		procedure Create( Complete : in out Complete_Module_Type ) is
		begin
			Create(
					Factory		=> Complete.Factory.all,
					Request		=> Request,
					Context		=> Page,
					Module_ID	=> Module_ID,
					Module		=> Complete.Module
				);
			Module_Id := Module_ID + 1;
		end Create;




		procedure Initialize( Complete : in out Complete_Module_Type ) is
		begin
			Initialize_Request(
					Module	=> Complete.Module.all,
					Request	=> Request,
					Config	=> Complete.Config
				);
		end Initialize;


		procedure Process_Head( Complete : in out Complete_Module_Type ) is
			Buffer : Unbounded_String;
		begin
			Process_Head(
					Module		=> Complete.Module.all,
					Request		=> Request,
					Response	=> Buffer
				);
			Append_Head(
					Processor	=> Processor,
					Region		=> Complete.Region,
					Module_ID	=> Get_ID( Complete.Module.all ),
					Head_Buffer	=> Buffer
				);
		end Process_Head;

		procedure Process_Body( Complete : in out Complete_Module_Type ) is
			Buffer : Unbounded_String;
		begin
			Process_Body(
					Module		=> Complete.Module.all,
					Request		=> Request,
					Response	=> Buffer
				);
			Append_Body(
					Processor	=> Processor,
					Region		=> Complete.Region,
					Module_ID	=> Get_ID( Complete.Module.all ),
					Body_Buffer	=> Buffer
				);
		end Process_Body;

		procedure Process_Foot( Complete : in out Complete_Module_Type ) is
			Buffer : Unbounded_String;
		begin
			Process_Foot(
					Module		=> Complete.Module.all,
					Request		=> Request,
					Response	=> Buffer
				);
			Append_Foot(
					Processor	=> Processor,
					Region		=> Complete.Region,
					Module_ID	=> Get_ID( Complete.Module.all ),
					Foot_Buffer	=> Buffer
				);
		end Process_Foot;

		procedure Finalize( Complete : in out Complete_Module_Type ) is
		begin
			Finalize_Request(
					Module	=> Complete.Module.all,
					Request	=> Request
				);
		end Finalize;

		procedure Destroy( Complete : in out Complete_Module_Type ) is
		begin
			Destroy(
					Factory		=> Complete.Factory.all,
					Request		=> Request,
					Module		=> Complete.Module
				);
		end Destroy;


	begin
		-------------------------
		-- Deal with modules.. --
		-------------------------
		Iterate( Modules => Modules, Iterator => Create'Access,		Require_Region => False );
		Iterate( Modules => Modules, Iterator => Initialize'Access,	Require_Region => False );
		Iterate( Modules => Modules, Iterator => Process_Head'Access,	Require_Region => True );
		Iterate( Modules => Modules, Iterator => Process_Body'Access,	Require_Region => True );
		Iterate( Modules => Modules, Iterator => Process_Foot'Access,	Require_Region => True );
		Iterate( Modules => Modules, Iterator => Finalize'Access,	Require_Region => False );
		Iterate( Modules => Modules, Iterator => Destroy'Access,	Require_Region => False );

		--------------------------------
		-- Assemble the final request --
		--------------------------------
		Process( Processor, Response );
	end Process_Custom_Request;






	function Get_Page(
				Service	: in Page_Service;
				Request	: in AWS.Status.Data
			) return String is
		-- retrieve the page name :)
	begin
		return KOW_View.Services.Util.Local_URI( Service, AWS.Status.URI( Request ) );
	end Get_Page;

	function Get_Config_File( Page : in String ) return KOW_Config.Config_File is
		-- get the config file for the given page..
		
		use KOW_Lib.File_System;

	begin
		return KOW_View.Components.Util.Load_Configuration(
						Component_Name		=> KOW_View.Components.Get_Name( KOW_View.Pages.Components.Component ),
						Configuration_Name	=> "page" / Page
					);
	end Get_Config_File;



	function Get_Modules( Config : in KOW_Config.Config_File ) return Complete_Module_Array is
		Empty : Complete_Module_Array( 1 .. 0 );
	begin
		-- TODO :: Get_Modules
		return Empty;
	end Get_Modules;


end KOW_View.Pages.Services;
