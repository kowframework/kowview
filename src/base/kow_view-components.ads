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
-- Component type declaration                                               --
--                                                                          --
-- A component is an abstract idea of a grouping of services and modules    --
--                                                                          --
-- Your component type should be suffixed by _component                     --
------------------------------------------------------------------------------


---------
-- Ada --
---------
with Ada.Directories;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_Lib.UString_Vectors;


---------
-- AWS --
---------
with AWS.Response;
with Templates_Parser;



package KOW_View.Components is


	----------------
	-- Exceptions --
	----------------
	COMPONENT_ERROR	: Exception;
	MODULE_ERROR	: Exception;
	SERVICE_ERROR	: Exception;



	-----------------------
	-- Service Delegator --
	-----------------------

	type Service_Delegator_Interface is interface;
	-- the service delegator is a singleton object that calls the service type methods
	-- see KOW_View.Services for more details.

	type Service_Delegator_Access is not null access all Service_Delegator_Interface'Class;
	type Service_Delegator_Ptr is access all Service_Delegator_Interface'Class;

	procedure Process_Custom_Request(
				Delegator	: in out Service_Delegator_Interface;
				Status		: in     Request_Status_Type;
				Response	:    out AWS.Response.Data
			) is abstract;

	procedure Process_Json_Request(
				Delegator	: in out Service_Delegator_Interface;
				Status		: in     Request_Status_Type;
				Response	:    out KOW_Lib.Json.Object_Type
			) is abstract;

	
	package Service_Delegator_Maps is new Ada.Containers.Ordered_Maps(
				Key_Type	=> Service_Name_Type,
				Element_Type	=> Service_Delegator_Ptr
			);



	------------
	-- Module --
	------------

	type Module_Interface is interface;
	-- so we know there is something called "module" at this level...
	-- this interface has no method

	type Module_Access is not null access all Module_Interface'Class;
	-- and where the module is actually stored..
	
	type Module_Ptr is access all Module_Interface'Class;




	procedure Initialize_Request(
			Module		: in out Module_Interface;
			Status		: in     Request_Status_Type
		) is null;
	-- Initialize the processing of a request
	-- Called before anything has been build.
	-- Useful when handling secured modules and setting initial data

	function Get_Script_Includes(
			Module		: in     Module_Interface
		) return KOW_Lib.UString_Vectors.Vector is abstract;

	function Get_AMDJS_Packages(
			Module		: in     Module_Interface
		) return KOW_Lib.UString_Vectors.Vector is abstract;
	
	function Get_AMDJS_CSS(
			Module		: in     Module_Interface
		) return KOW_Lib.UString_Vectors.Vector is abstract;

	function Get_CSS_Includes(
			Module		: in     Module_Interface
		) return KOW_Lib.UString_Vectors.Vector is abstract;



	procedure Process_Body(
			Module		: in out Module_Interface;
			Status		: in     Request_Status_Type;
			Response	:    out Unbounded_String
		) is null;
	-- process the request for a module.
	-- sometimes is useful for a module only to be created and released - such as in a page counter module


	procedure Process_Json_Request(
			Module		: in out Module_Interface;
			Status		: in     Request_Status_Type;
			Response	: out    KOW_Lib.Json.Object_Type
		) is null;

	procedure Finalize_Request(
			Module		: in out Module_Interface;
			Status		: in     Request_Status_Type
		) is null;
	-- Finalize processing the request.
	-- Called when the process has been finalized



	function Get_ID( Module : in Module_Interface ) return Positive is abstract;
	-- get the module ID inside a given page

	--------------------
	-- Module Factory --
	--------------------

	type Module_Factory_Interface is interface;
	-- the module factory is a singleton object that produce and destroy modules
	-- see KOW_View.Modules for more details

	type Module_Factory_Access is not null access all Module_Factory_Interface'Class;
	type Module_Factory_Ptr is access all Module_Factory_interface'Class;

	procedure Create(
				Factory		: in out Module_Factory_Interface;
				Status		: in     Request_Status_Type;
				Context		: in     String;
				Module_Id	: in     Positive;
				Request_Mode	: in     Request_Mode_Type;
				Virtual_Host	: in     KOW_View.Virtual_Host_Name_Type;
				Module		:    out Module_Ptr
			) is abstract;
	-- create a module, setting it's ID if necessary
	-- Context	=> string representing something like the page where the module will be created
	-- Module_ID	=> which module in sequence being created in the same context

	procedure Destroy(
				Factory		: in out Module_Factory_Interface;
				Status		: in     Request_Status_Type;
				Module		: in out Module_Ptr
			) is abstract;
	-- free the module access type


	package Module_Factory_Maps is new Ada.Containers.Ordered_Maps(
				Key_Type	=> Module_Name_Type,
				Element_Type	=> Module_Factory_Ptr
			);

	--------------------------------------
	-- Component Initialization Trigger --
	--------------------------------------

	type Initialization_Trigger_Access is not null access procedure;
	type Initialization_Trigger_Ptr is access procedure;

	package Initialization_Trigger_Vectors is new Ada.Containers.Vectors(
				Index_Type	=> Positive,
				Element_Type	=> Initialization_Trigger_Ptr
			);

	----------------
	-- Components --
	----------------

	type Component_Type is abstract tagged record
		Service_Delegators	: Service_Delegator_Maps.Map;
		-- where I look for my services..

		Module_Factories	: Module_Factory_Maps.Map;
		-- where I look for how to create and destroy modules

		Default_Service		: Service_Delegator_Ptr;
		-- the component implementor must set this variable to whatever service he
		-- wants as the default service. :)

		Initialization_Triggers	: Initialization_Trigger_Vectors.Vector;
		-- durin elaboration your code can register initialization triggers
		-- that will be called by the initialize 
	end record;

	type Component_Access is not null access all Component_Type'Class;
	-- whenever possible use Component_Access as your pointer type

	type Component_Ptr is access all Component_Type'Class;
	-- the component_ptr type was created so we could easily implement the
	-- 	. service.component
	-- 	. module.component
	-- variables



	procedure Setup(
			Component	: in out Component_Type;
			Config		: in out KOW_Config.Config_File_Type
		) is null;
	-- setup the component while starting up the server
	-- Config is an already initialized configuration file located at:
	-- 	kowview/component_name
	--
	-- override this procedure whenever you need to write your initialization code



	procedure Initialize(
			Component		: in out Component_Type;
			Require_Configuration	: in     Boolean
		);
	-- initialize the request doind:
	-- 	1. call the abstract setup procedure
	-- 	2. runing all the initialization triggers
	--
	-- as a side note, if Require_Configuration = true and the configuration file is not found
	-- raise COMPONENT_ERROR with a nice message
	--
	-- otherwise Setup is aways run, even if with an empty configuration file


	function Locate_Resource(
			Component	: in Component_Type;
			Resource	: in String;
			Extension	: in String := "";
			Virtual_Host	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return String;
	-- locate a resource file using KOW_View.Components.Util.Locate_Resource


	procedure Register_Service_Delegator(
			Component	: in out Component_Type;
			Name		: in     String;
			Delegator	: in     Service_Delegator_Access
		);

	procedure Register_Service_Delegator(
			Component	: in out Component_Type;
			Name		: in     Service_Name_Type;
			Delegator	: in     Service_Delegator_Access
		);
	-- register a new service delegator...
	-- the name of this delegator is going to be calculated from the 


	function Get_Service_Delegator(
			Component	: in Component_Type;
			Service		: in Service_Name_Type
		) return Service_Delegator_Access;
	-- return the service delegator for this request..
	-- you should override this method in case you want only one service in your component 
		
	procedure Process_Json_Request(
			Component	: in out Component_Type;
			Status		: in     Request_Status_Type;
			Response	:    out KOW_Lib.Json.Object_Type
		);

	procedure Process_Custom_Request(
			Component	: in out Component_Type;
			Status		: in     Request_Status_Type;
			Response	:    out AWS.Response.Data
		);
	-- this is where the request processing takes place..
	-- can be overriding for implementing default services and such


	procedure Register_Module_Factory(
			Component	: in out Component_Type;
			Name		: in     Module_Name_Type;
			Factory		: in     Module_Factory_Access
		);
	
	function Get_Module_Factory(
			Component	: in Component_Type;
			Name		: in Module_Name_Type 
		) return Module_Factory_Access;

	procedure Register_Initialization_Trigger(
				Component		: in out Component_Type;
				Initialization_Trigger	: in     Initialization_Trigger_Access
			);



	function Get_Name( Component : in Component_Type'Class ) return String;



end KOW_View.Components;
