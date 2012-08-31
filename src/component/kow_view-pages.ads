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
with Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_View.Services;
with KOW_View.Themes;


---------
-- AWS --
---------
with AWS.Response;


package KOW_View.Pages is


	------------------------
	-- The Page Interface --
	------------------------

	type Page_Interface is interface;
	-- for the Module, a page is just an interface that implement some methods it uses
	--
	-- also, the page is responsible for handling the modules it contains


	procedure Append(
				Page	: in out Page_Interface;
				Content	: in     String
			) is abstract;
	-- append some content into the page
	-- this is a callback for the module

	procedure Include_Script(
				Page	: in out Page_Interface;
				Script	: in     String
			) is abstract;
	-- include a script into the page

	procedure Include_CSS(
				Page	: in out Page_Interface;
				CSS	: in     String
			) is abstract;
	-- include a CSS file into the page



	procedure Include_AMDJS_Module(
				Page	: in out Page_Interface;
				Module	: in     String
			) is abstract;
	-- include an AMDJS module into the page.
	-- which amdjs implementation to use is up to the Theme engine.
	


	function Get_Module_Id(
				Page	: in Page_Interface
			) return String is abstract;
	-- return a string that can be used to identify the current module

	---------------------
	-- The Module Type --
	---------------------

	type Module_Interface is interface;
	-- the module represents a small part of the page

	type Module_Ptr is access all Module_Interface'Class;


	procedure Process_Request(
				Module	: in out Module_Interface;
				Page	: in out Page_Interface'Class;
				Status	: in     Request_Status_Type
			) is abstract;
	-- process the HTML body for the given request
	-- the body should be included in the page using the Append method.


	procedure Process_Json_Request(
				Module	: in out Module_Interface;
				Page	: in out Page_interface'Class;
				Status	: in     Request_Status_Type;
				Response:    out KOW_Lib.Json.Object_Type
			) is abstract;
	-- called when processing json requests by the page



	-----------------------------
	-- The Module Factory Type --
	-----------------------------


	type Module_Factory_Interface is interface;
	-- the module factory interface is responsible for controlling the module
	-- life cycle.

	type Module_Factory_Ptr is access all Module_Factory_Interface'Class;
	type Module_Factory_Array is array( Positive range <> ) of Module_Factory_Ptr;



	procedure Create(
				Factory	: in out Module_Factory_Interface;
				Status	: in     Request_Status_Type;
				Module	:    out Module_Ptr
			) is abstract;
	-- return an initialized access to a module

	procedure Destroy(
				Factory	: in out Module_Factory_Interface;
				Status	: in     Request_Status_Type;
				Module	: in out Module_Ptr
			) is abstract;
	-- deallocate the given module



	------------------
	-- Page Service --
	------------------

	type Page_Title is access String;

	generic
		type Regions is (<>);
	package Base is

		Theme_Engine : KOW_View.Themes.Theme_Engine_Ptr := KOW_View.Themes.Default;
		-- this can be overriden by the implementation


		type Page_Type is abstract new KOW_View.Services.Service_Type and Page_Interface with record
			Title			: Page_Title;
			-- the page title

			Template		: KOW_View.Themes.Template_Name;

			Scripts, CSSs, AMDJS	: KOW_Lib.Json.Array_Type;
			-- store the things to be included


			Current_Region	: Regions;
			Current_Index	: Positive;
			-- map the current module and region


			Buffer		: Ada.Strings.Unbounded.Unbounded_String;
			-- a buffer that's reutilized
		end record;



		-- 
		-- Service Methods 
		-- 

		overriding
		function Get_Name(
				Page	: in Page_Type
			) return Service_Name;
		-- get the page name (based on your own type's name)
		-- the type should be [PAGE NAME]_Page


		overriding
		procedure Process_Custom_Request(
					Page		: in out Page_Type;
					Status		: in     KOW_View.Request_Status_Type;
					Response	:    out AWS.Response.Data
				);

		overriding
		procedure Process_Json_Request(
					Page		: in out Page_Type;
					Status		: in     KOW_View.Request_Status_Type;
					Response	:    out KOW_Lib.Json.Object_Type
				);
	
		--
		-- Page Interface Methods
		--

		overriding
		procedure Append(

				Page	: in out Page_Type;
				Content	: in     String
			);
		-- append some content into the page
		-- this is a callback for the module

		overriding
		procedure Include_Script(
					Page	: in out Page_Type;
					Script	: in     String
				);
		-- include a script into the page

		overriding
		procedure Include_CSS(
					Page	: in out Page_Type;
					CSS	: in     String
				);
		-- include a CSS file into the page



		overriding
		procedure Include_AMDJS_Module(
					Page	: in out Page_Type;
					Module	: in     String
				);
		-- include an AMDJS module into the page.
		-- which amdjs implementation to use is up to the Theme engine.
		


		function Get_Module_Id(
					Page	: in Page_Type
				) return String;
		-- return a string that can be used to identify the current module


		-- 
		-- New Methods
		--

		procedure Split(
					Page	: in     Page_Type;
					Context	: in     Name_Type;
					Region	:    out Regions;
					Index	:    out Positive
				);



		function Get_Module_Factories(
					Page	: in Page_Type;
					Region	: in Regions
				) return Module_Factory_Array is abstract;

	end Base;


	

end KOW_View.Pages;
