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
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.File_System;
with KOW_Lib.Json;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;				use KOW_View.Components;
with KOW_View.Components.Registry;
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
with AWS.Parameters;
with AWS.Response;
with AWS.Status;


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

		Params		: AWS.Parameters.List := AWS.Status.Parameters( Request );


		Page		: constant String := Get_Page( Service, Request );
		Json_Module_Str	: constant String := AWS.Parameters.Get( Params, "module_id" );
		Config		: KOW_Config.Config_File_Type := Util.Get_Config_File( Page );
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
					Request_Mode	=> Json_Request,
					Virtual_Host	=> KOW_View.Virtual_Host( Request ),
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
		if Json_Module_Str = "" then
			raise CONSTRAINT_ERROR with "I don't know which Json request to process! Sorry!";
		end if;

		begin
			-- we put this begin block here so the exception above won't be cauch:
			Setup( Service, Config );
	
			Iterate( Modules => Modules, Iterator => Create'Access );
			Iterate( Modules => Modules, Iterator => Initialize'Access );
	
			-- TODO :: see wich json module should be called :)
	
			KOW_View.Components.Process_Json_Request(
						Module	=> Modules( Integer'Value( Json_Module_Str ) ).Module.all,
						Request	=> Request,
						Response=> Response
					);
		
			Iterate( Modules => Modules, Iterator => Finalize'Access );
			Iterate( Modules => Modules, Iterator => Destroy'Access );
		
		exception
			when e : others =>
				Iterate( Modules => Modules, Iterator => Destroy'Access );
				Ada.Exceptions.Reraise_Occurrence( e );
		end;	
	end Process_Json_Request;


	overriding
	procedure Process_Custom_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		) is
	begin
		Process_Custom_Request(
					Service		=> Service,
					Request		=> Request,
					Response	=> Response,
					Page		=> Get_Page( Service, Request ),
					Initialize_Only	=> False
				);
	end Process_Custom_Request;

	procedure Process_Custom_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data;
			Page		: in     String;
			Initialize_Only	: in     Boolean
		) is
		-- process the entire module cycle returning a HTML page

		use KOW_View.Themes.Template_Processors;

		Config		: KOW_Config.Config_File_Type := Util.Get_Config_File( Page );
		Template	: KOW_View.Themes.Template_Type := Util.Get_Template( Config );

		Modules		: Complete_Module_Array := Util.Get_Modules( Config );
		type Buffer_Array is Array( 1 .. Modules'Length ) of Unbounded_String;
		Body_Buffers	: Buffer_Array;

		type Process_Module_Array is array( Buffer_Array'Range ) of Boolean;
		Process_Module	: Process_Module_Array := ( others => false );

		Processor	: Template_Processor_Type := New_Template_Processor( Template );

		Module_Id	: Positive := 1;


		Virtual_Host	: constant KOW_View.Virtual_Host_Name_Type := KOW_View.Virtual_Host( Request );
		procedure Create( Complete : in out Complete_Module_Type ) is
		begin
			Create(
					Factory		=> Complete.Factory.all,
					Request		=> Request,
					Context		=> Page,
					Module_ID	=> Module_ID,
					Request_mode	=> Custom_Request,
					Virtual_Host	=> Virtual_Host,
					Module		=> Complete.Module
				);
			Module_Id := Module_ID + 1;
		end Create;


		procedure Initialize_Process_Module( C : in KOW_Lib.UString_Vectors.Cursor ) is
			use KOW_View.Themes;
			Region		: Region_Type := Region_Type( KOW_Lib.UString_Vectors.Element( C ) );
			Module_IDs	: Index_Array := Util.Get_Module_IDs( Config, Region );
		begin
			for i in Module_IDs'Range loop
				Process_Module( Module_IDs( i ) ) := true;
			end loop;
		end Initialize_Process_Module;


		procedure Initialize( Complete : in out Complete_Module_Type ) is
		begin
			Initialize_Request(
					Module	=> Complete.Module.all,
					Request	=> Request,
					Config	=> Complete.Config
				);
		end Initialize;



		procedure Process_Body( Complete : in out Complete_Module_Type ) is
			Id : constant Natural := Get_ID( Complete.Module.all );
		begin
			if Process_Module( id ) then
				Process_Body(
						Module		=> Complete.Module.all,
						Request		=> Request,
						Response	=> Body_Buffers( id )
					);
			end if;
		end Process_Body;


		procedure Process_Script_Includes( Complete : in out Complete_Module_Type ) is
		begin
			Append_Script_Includes(
					Processor	=> Processor,
					Script_Includes	=> Get_Script_Includes( Complete.Module.all )
				);
		end Process_Script_Includes;

		procedure Process_Dojo_Packages( Complete : in out Complete_Module_Type ) is
		begin
			Append_Dojo_Packages(
					Processor	=> Processor,
					Dojo_Packages	=> Get_Dojo_Packages( Complete.Module.all )
				);
		end Process_Dojo_Packages;


		procedure Process_Dojo_CSS( Complete : in out Complete_Module_Type ) is
		begin
			Append_Dojo_CSS(
					Processor	=> Processor,
					Dojo_CSS	=> Get_Dojo_CSS( Complete.Module.all )
				);
		end Process_Dojo_CSS;

		procedure Process_CSS_Includes( Complete : in out Complete_Module_Type ) is
		begin
			Append_CSS_Includes(
					Processor	=> Processor,
					CSS_Includes	=> Get_CSS_Includes( Complete.Module.all )
				);
		end Process_CSS_Includes;


		procedure Finalize( Complete : in out Complete_Module_Type ) is
		begin
			Finalize_Request(
					Module	=> Complete.Module.all,
					Request	=> Request
				);
		end Finalize;

		procedure Destroy( Complete : in out Complete_Module_Type ) is
		begin
			if Complete.Module /= null then
				Destroy(
						Factory		=> Complete.Factory.all,
						Request		=> Request,
						Module		=> Complete.Module
					);
			end if;
		end Destroy;




		procedure Append_Region( C : in KOW_Lib.UString_Vectors.Cursor ) is
			use KOW_View.Themes;
			Region		: Region_Type := Region_Type( KOW_Lib.UString_Vectors.Element( C ) );
			Module_IDs	: Index_Array := Util.Get_Module_IDs( Config, Region );
			Module_ID	: Positive;
		begin
			for i in Module_IDs'Range loop
				Module_ID := Module_IDs( i );
				Append_Body(
						Processor	=> Processor,
						Region		=> Region,
						Module_ID	=> Module_ID,
						Body_Buffer	=> Body_Buffers( Module_ID )
					);
			end loop;
		end Append_Region;


		-- I copy those to avoid pointer madness
		Page_Scr : KOW_Lib.UString_Vectors.Vector := Page_Script_Includes;
		Dojo_Pkg : KOW_Lib.UString_Vectors.Vector := Page_Dojo_Packages;
	begin
		Setup( Service, Config );

		------------------------------------------------------
		-- Deal with required Page includes except for dojo --
		------------------------------------------------------
		Append_Script_Includes(
				Processor	=> Processor,
				Script_Includes	=> Page_Scr
			);

		Append_Dojo_Packages(
				Processor	=> processor,
				Dojo_Packages	=> Dojo_Pkg
			);

		-------------------------
		-- Deal with modules.. --
		-------------------------
		Iterate( Modules => Modules, Iterator => Create'Access );
		Iterate( Modules => Modules, Iterator => Initialize'Access );	

		KOW_Config.Set_Section( Config, "positions" );
		KOW_Lib.UString_Vectors.Iterate( Template.Regions, Initialize_Process_Module'Access );
		-- this will initialize the Process_Module array which will tell
		-- what modules should have body

		if not Initialize_Only then
	
			Iterate( Modules => Modules, Iterator => Process_Body'Access );

			Iterate( Modules => Modules, Iterator => Process_Script_Includes'Access );
			Iterate( Modules => Modules, Iterator => Process_Dojo_Packages'Access );
			Iterate( Modules => Modules, Iterator => Process_Dojo_CSS'Access );
			Iterate( Modules => Modules, Iterator => Process_CSS_Includes'Access );
		end if;
		Iterate( Modules => Modules, Iterator => Finalize'Access );
		Iterate( Modules => Modules, Iterator => Destroy'Access );

		--------------------------------
		-- Assemble the final request --
		--------------------------------
		KOW_Lib.UString_Vectors.Iterate( Template.Regions, Append_Region'Access );

		Processor.Title := Service.Title;
		Processor.Author := Service.Author;

		Process( Processor, Request, Response );
	exception
		when e : others =>
			-- remember to destroy...
			Iterate( Modules => Modules, Iterator => Destroy'Access );
			Ada.Exceptions.Reraise_Occurrence( e );
	end Process_Custom_Request;






	function Get_Page(
				Service	: in Page_Service;
				Request	: in AWS.Status.Data
			) return String is
		-- retrieve the page name :)
		Page : constant string := KOW_View.Services.Util.Local_URI( Service, AWS.Status.URI( Request ) );
	begin
		if page = "" then
			return "main";
		else
			return page;
		end if;
	end Get_Page;


	procedure Setup(
				Service	: in out Page_Service;
				Config	: in out KOW_Config.Config_File_Type
			) is
	begin
		Service.Title	:= To_Unbounded_String( KOW_Config.Default_Value( Config, "title" ) );
		Service.Author	:= To_Unbounded_String( KOW_Config.Default_Value( Config, "author" ) );
	end Setup;

	--------------------------------------
	-- The Module Resource Service Type --
	--------------------------------------


	overriding
	function Locate_Resource(
			Service		: in Component_Resource_Service_Type;
			Resource	: in String;
			Extension	: in String := "";
			Virtual_Host	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return String is
		-- locate resource given:
		-- 	when resource is a URN using:
		-- 		component:component/somefile
		-- use the locate resource implementation for the given component, prefixing the resource by get_name(service)
		--
		-- or else use the locate resource for the current component
		use KOW_Lib.File_System;
		
		Comp	: constant String := "component:";
		Last	: constant Integer := Ada.Strings.Fixed.Index( Source => Resource, Pattern => "/" );
		Prefix	: constant String := KOW_View.Services.Get_Name( Service );


		function Get_Component return Component_Access is
			Comp_Name : constant String := Resource( Resource'First + Comp'Length .. Last - 1 );
		begin
			return KOW_View.Components.Registry.Get_Component( Comp_Name );
		end Get_Component;

		function Get_Resource return String is
		begin
			return Resource( Last + 1 .. Resource'Last );
		end Get_Resource;
	begin

		if Resource'Length > Comp'Length and then Resource( Resource'First .. Resource'First + Comp'Length - 1 ) = Comp then
			return Locate_Resource(
						Component	=> Get_Component.all,
						Resource	=> Prefix / Get_Resource,
						Extension	=> Extension,
						Virtual_Host	=> Virtual_Host,
						Kind		=> Kind,
						Locale		=> Locale
					);
		else
			return Locate_Resource(
						Component	=> Service.Component.all,
						Resource	=> Prefix / Resource,
						Extension	=> Extension,
						Virtual_Host	=> Virtual_Host,
						Kind		=> Kind,
						Locale		=> Locale
					);
		end if;
	end Locate_Resource;










	-----------------
	-- Dir Service --
	-----------------



	overriding
	procedure Process_Json_Request(
			Service	: in out Dir_Service;
			Request	: in     AWS.Status.Data;
			Response:    out KOW_Lib.Json.Object_Type
		) is
		use KOW_Lib.Json;
		use KOW_View.Components;
		use KOW_View.Components.Registry;


		Components : Array_Type;

		procedure Component_Iterator( C : in Component_Maps.Cursor ) is
			Component_Name	: Unbounded_String := Component_Maps.Key( C );
			Component	: Component_Ptr := Component_Maps.Element( C );

			Component_object: Object_Type;
			Services	: Array_Type;
			Modules		: Array_Type;


			procedure Service_Iterator( S_C : in Service_Delegator_Maps.Cursor ) is
			begin
				Append( Services, Service_Delegator_Maps.Key( S_C ) );
			end Service_Iterator;

			procedure Module_Iterator( M_C : in Module_Factory_Maps.Cursor ) is
			begin
				Append( Modules, Module_Factory_Maps.Key( M_C ) );
			end Module_Iterator;
		begin

			Service_Delegator_Maps.Iterate( Component.Service_Delegators, Service_Iterator'Access );
			Module_Factory_Maps.Iterate( Component.Module_Factories, Module_Iterator'Access );

			
			Set( Component_Object, "name", component_name );
			Set( Component_Object, "services", Services );
			Set( Component_Object, "modules", Modules );

			Append( Components, Component_Object );

		end Component_Iterator;
	begin
		Component_Maps.Iterate( Get_Components, Component_Iterator'Access );


		declare
			Obj : Object_Type;
		begin
			Set( Obj, "components", Components );
			Response := Obj;
		end;
		
	end Process_Json_Request;



	overriding
	procedure Process_Custom_Request(
			Service		: in out Dir_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		) is
		use Ada.Strings.Unbounded;
		use KOW_View.Components;
		use KOW_View.Components.Registry;


		Buffer : Unbounded_String;

		procedure Component_Iterator( C : in Component_Maps.Cursor ) is
			Component_Name	: Unbounded_String := Component_Maps.Key( C );
			Component	: Component_Ptr := Component_Maps.Element( C );

			procedure Service_Iterator( S_C : in Service_Delegator_Maps.Cursor ) is
			begin
				Append( Buffer, "<li>" );
				Append( Buffer, Service_Delegator_Maps.Key( S_C ) );
				Append( Buffer, "</li>" );
			end Service_Iterator;

			procedure Module_Iterator( M_C : in Module_Factory_Maps.Cursor ) is
			begin
				Append( Buffer, "<li>" );
				Append( Buffer, Module_Factory_Maps.Key( M_C ) );
				Append( Buffer, "</li>" );
			end Module_Iterator;
		begin

			Append( Buffer, "<h2>" );
			Append( Buffer, Component_Name );
			Append( Buffer, "</h2>" );

			Append( Buffer, "<h3>Services:</h3>" );
			Append( Buffer, "<ul>" );
				Service_Delegator_Maps.Iterate( Component.Service_Delegators, Service_Iterator'Access );
			Append( Buffer, "</ul>" );

			Append( Buffer, "<h3>Modules:</h3>" );
			Append( Buffer, "<ul>" );
				Module_Factory_Maps.Iterate( Component.Module_Factories, Module_Iterator'Access );
			Append( Buffer, "</ul>" );
		end Component_Iterator;


	begin
		Append( Buffer, "<html><head><title>Available Components</title></head><body><h1>Available Components</h1>" );
		Append( Buffer, "This is an automatically generated page with all available components and it's services and modules." );
		Component_Maps.Iterate( Get_Components, Component_Iterator'Access );
		Append( Buffer, "</body></html>" );
		Response := AWS.Response.Build(
					Content_Type	=> "text/html",
					Message_Body	=> To_String( Buffer )
				);

	end Process_Custom_Request;










	procedure include( Str : in String ) is
	begin
		KOW_Lib.UString_Vectors.Append( Page_Script_Includes, To_Unbounded_String( Str ) );
	end include;

	procedure include_dojo( Str : in String ) is
	begin
		KOW_Lib.UString_Vectors.Append( Page_Dojo_Packages, To_Unbounded_String( Str ) );
	end include_dojo;
begin
	include( "/pages/js/kowview.js" );
	include( "/pages/js/kowview-messages.js" );
	include( "/pages/js/kowview-modules.js" );
	include( "/pages/js/kowview-services.js" );


	include_dojo( "dijit.Dialog" );
	include_dojo( "dijit.form.Button" );
	include_dojo( "dijit.ProgressBar" );
	include_dojo( "dojo.io.iframe" ); -- used by the kowview-entities :)
end KOW_View.Pages.Services;
