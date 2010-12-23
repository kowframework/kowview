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
with KOW_Lib.UString_Vectors;
with KOW_View.Components;				use KOW_View.Components;
with KOW_View.Components.Util;
with KOW_View.Pages.Components;
with KOW_View.Pages.Services.Util;
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
				Iterator	: access procedure( Module : in out Complete_Module_Type )
			) is
		use KOW_View.Themes;
	begin
		for i in Modules'Range loop 
			Iterator.all( Modules( i ) );
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
		Config		: KOW_Config.Config_File := Util.Get_Config_File( Page );
		Template	: KOW_View.Themes.Template_Type := Util.Get_Template( Config );

		Modules		: Complete_Module_Array := Util.Get_Modules( Config );
		Processor	: Template_Processor_Type := New_Template_Processor( Template );

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
		Setup( Service, Config );

		Iterate( Modules => Modules, Iterator => Create'Access );
		Iterate( Modules => Modules, Iterator => Initialize'Access );

		-- see wich json module should be called :)

		Iterate( Modules => Modules, Iterator => Finalize'Access );
		Iterate( Modules => Modules, Iterator => Destroy'Access );


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
		Config		: KOW_Config.Config_File := Util.Get_Config_File( Page );
		Template	: KOW_View.Themes.Template_Type := Util.Get_Template( Config );

		Modules		: Complete_Module_Array := Util.Get_Modules( Config );
		type Buffer_Array is Array( 1 .. Modules'Length ) of Unbounded_String;
		Head_Buffers	: Buffer_Array;
		Body_Buffers	: Buffer_Array;
		Foot_Buffers	: Buffer_Array;

		Processor	: Template_Processor_Type := New_Template_Processor( Template );

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
		begin
			Process_Head(
					Module		=> Complete.Module.all,
					Request		=> Request,
					Response	=> Head_Buffers( Get_ID( Complete.Module.all ) )
				);
		end Process_Head;

		procedure Process_Body( Complete : in out Complete_Module_Type ) is
		begin
			Process_Body(
					Module		=> Complete.Module.all,
					Request		=> Request,
					Response	=> Head_Buffers( Get_ID( Complete.Module.all ) )
				);
		end Process_Body;

		procedure Process_Foot( Complete : in out Complete_Module_Type ) is
			Buffer : Unbounded_String;
		begin
			Process_Foot(
					Module		=> Complete.Module.all,
					Request		=> Request,
					Response	=> Foot_Buffers( Get_ID (Complete.Module.all ) )
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




		procedure Append_Region( C : in KOW_Lib.UString_Vectors.Cursor ) is
			use KOW_View.Themes;
			Region		: Region_Type := Region_Type( KOW_Lib.UString_Vectors.Element( C ) );
			Module_IDs	: Index_Array := Util.Get_Module_IDs( Config, Region );
			Module_ID	: Positive;
		begin
			for i in Modules'Range loop
				Module_ID := Module_IDs( i );
				Append_Head(
						Processor	=> Processor,
						Region		=> Region,
						Module_Id	=> Module_ID,
						Head_Buffer	=> Head_Buffers( Module_ID )
					);
				Append_Body(
						Processor	=> Processor,
						Region		=> Region,
						Module_ID	=> Module_ID,
						Body_Buffer	=> Body_Buffers( Module_ID )
					);
				Append_Foot(
						Processor	=> Processor,
						Region		=> Region,
						Module_ID	=> Module_ID,
						Foot_Buffer	=> Foot_Buffers( Module_ID )
					);
			end loop;
		end Append_Region;


	begin
		Setup( Service, Config );
		-------------------------
		-- Deal with modules.. --
		-------------------------
		Iterate( Modules => Modules, Iterator => Create'Access );
		Iterate( Modules => Modules, Iterator => Initialize'Access );	
		Iterate( Modules => Modules, Iterator => Process_Head'Access );
		Iterate( Modules => Modules, Iterator => Process_Body'Access );
		Iterate( Modules => Modules, Iterator => Process_Foot'Access );
		Iterate( Modules => Modules, Iterator => Finalize'Access );
		Iterate( Modules => Modules, Iterator => Destroy'Access );

		--------------------------------
		-- Assemble the final request --
		--------------------------------
		KOW_Lib.UString_Vectors.Iterate( Template.Regions, Append_Region'Access );

		Processor.Page_Title := Service.Page_Title;
		Processor.Author := Service.Author;

		Process( Processor, Request, Response );
	end Process_Custom_Request;






	function Get_Page(
				Service	: in Page_Service;
				Request	: in AWS.Status.Data
			) return String is
		-- retrieve the page name :)
	begin
		return KOW_View.Services.Util.Local_URI( Service, AWS.Status.URI( Request ) );
	end Get_Page;


	procedure Setup(
				Service	: in out Page_Service;
				Config	: in out KOW_Config.Config_File
			) is
	begin
		Service.Page_Title	:= KOW_Config.Element( Config, "page_title" );
		Service.Author		:= KOW_Config.Element( Config, "author" );
	end Setup;



end KOW_View.Pages.Services;
