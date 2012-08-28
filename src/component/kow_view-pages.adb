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
------------------------------------------------------------------------------
pragma License (GPL);

------------------------------------------------------------------------------
-- Main package for KOW_View                                                --
------------------------------------------------------------------------------




--------------
-- Ada 2005 --
--------------
with Ada.Exceptions;
with Ada.Strings.Unbouned;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_View.Services;
with KOW_View.Util;


---------
-- AWS --
---------
with AWS.Response;


package body KOW_View.Pages is



	------------------
	-- Page Service --
	------------------

	generic
		type Regions is (<>);
		Theme_Engine : KOW_View.Themes.Theme_Engine_Ptr;
	package body Base is
		type Page_Type is new abstract KOW_View.Services.Service_Type and Page_Interface with record
			Title			: Page_Title;
			-- the page title


			Scripts, CSSs, AMDJS	: KOW_Lib.Json.Array_Type;
			-- store the things to be included


			Current_Region	: Regions;
			Current_Index	: Positive;
			-- map the current module and region
		end record;


		-- 
		-- Service Methods 
		-- 

		overriding
		function Get_Name(
				Page	: in Page_Type
			) return Service_Name is
			-- get the page name (based on your own type's name)
			-- the type should be [PAGE NAME]_Page
		begin
			return KOW_View.Util.Get_Type_Name( Page_Type'Class( Page )'Tag, "_page" );
		end Get_Name;


		overriding
		procedure Process_Custom_Request(
					Page		: in out Page_Type;
					Status		: in     KOW_View.Request_Status_Type;
					Response	:    out AWS.Response.Data
				) is
			Initial_State	: KOW_Lib.Json.Object_Type;
			Module		: Module_Ptr;
		begin
			KOW_Lib.Json.Set( Initial_State, "title", Page.Title.all );

			for Page.Current_Region in Regions'Range loop
				declare
					Factories : Module_Factory_Array := Get_Module_Factories(
													Page	=> Page_Type'Class( Page ),
													Region	=> Page.Current_Region
												);
					Contents  : KOW_Lib.Json.Array_Type;
				begin
					for Page.Current_Index in Factories'Range loop
						Create( Factories( Page.Current_Index ).all, Status, Module );
						pragma Assert( Module /= null, "The factory is not allocating the correct module!" );

						Page.Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
						Process_Request( Module.all, Page, Status );
						KOW_Lib.Json.Append( Contents, Page.Buffer );

						Destroy( Factories( Page.Current_Index ).all, Status, Module );
						pragma Assert( Module = null, "The factory is not deallocating the module! Memory leak?" );
					end loop;

					KOW_Lib.Json.Set( Initial_State, Regions'Image( Page.Current_Region ) & "_CONTENTS", Contents );
				end;
			end loop;


			KOW_View.Themes.Build_Response(
							Theme_Engine	=> Theme_Engine.all,
							Service		=> Page,
							Status		=> Status,
							Template	=> Page.Template,
							Initial_State	=> Initial_State,
							Response	=> Response
						);
		end Process_Custom_Request;



		overriding
		procedure Process_Json_Request(
					Page		: in out Page_Type;
					Status		: in     KOW_View.Request_Status_Type;
					Response	:    out KOW_Lib.Json.Object_Type
				) is
			Factory	: Module_Factory_Ptr;
			Module	: Module_Ptr;
		begin
			Split(
					Page	=> Page_Type'Class( Page ),
					Context	=> Status.Context,
					Region	=> Page.Current_Region,
					Index	=> Page.Current_Index
				);

			Factory := Get_Modules( 
						Page	=> Page_Type'Class( Page ),
					       	Region	=> Page.Current_Region
					)( Page.Currend_Index );

			Create( Factory.all, Status, Module );

			Process_Json_Request(
						Module	=> Module.all,
						Status	=> Status,
						Response=> Response
					);

			Destroy( Factory.all, Status, Module );
		exception
			when e : others =>
				if Module /= null then
					Destroy( Factory.all, Status, Module );
				end if;
				Ada.Exceptions.Reraise_Occurrence( e );

		end Process_Json_Request;
	
		--
		-- Page Interface Methods
		--

		overriding
		procedure Append(
				Page	: in out Page_Type;
				Content	: in     String
			) is
			-- append some content into the page
			-- this is a callback for the module
		begin
			Ada.Strings.Unbounded.Append( Page.Buffer, Content );
		end Append;


		overriding
		procedure Include_Script(
					Page	: in out Page_Type;
					Script	: in     String
				) is
			-- include a script into the page
		begin
			Append_Unique( Page.Scripts, Script );
		end Include_Script;

		overriding
		procedure Include_CSS(
					Page	: in out Page_Type;
					CSS	: in     String
				) is
			-- include a CSS file into the page
		begin
			Append_Unique( Page.CSS, CSS );
		end Include_CSS;


		overriding
		procedure Include_AMDJS_Module(
					Page	: in out Page_Type;
					Module	: in     String
				) is
			-- include an AMDJS module into the page.
			-- which amdjs implementation to use is up to the Theme engine.
		begin
			Append_Unique( Page.AMDJS, Module );
		end Include_AMDJS_Module;


		function Get_Module_Id(
					Page	: in Page_Type
				) return String is
			-- return a string that can be used to identify the current module
		begin
			return Regions'Image( Page.Current_Region ) & "_" & Ada.Strings.Fixed.Trim( Positive'Image( Page.Current_Index ), Ada.Strings.Both );
		end Get_Module_Id;

		-- 
		-- New Methods
		--

		procedure Split(
					Page	: in     Page_Type;
					Context	: in     Name_Type;
					Region	:    out Regions;
					Index	:    out Positive
				) is
			SContext	: constant String := To_String( Context );
			Idx 		: constant Integer := Ada.Strings.Fixed.Index( SContext, ":" );
		begin
			if Idx not in Context'Range then
				raise CONSTRAINT_ERROR with SContext & " is not a valid context for this request.";
			end if;
			Region := Regions'Value( SContext( SContext'First .. Idx - 1 ) );
			Index := Positive'Value( SContext( Idx + 1 .. SContext'Last ) );
		end Split;


	end Base;


	

end KOW_View.Pages;
