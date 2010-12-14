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


---------
-- AWS --
---------
with AWS.Response;
with AWS.Status;
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
	-- the service delegator is the object that actually process the request

	type Service_Delegator_Access is access all Service_Delegator_Interface'Class;

	procedure Process_Custom_Request(
				Service : in out Service_Delegator_Interface;
				Request	: in     AWS.Status.Data;
				Response:    out AWS.Response.Data
			) is abstract;

	procedure Process_Json_Request(
				Service	: in out Service_Delegator_Interface;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			) is abstract;

	
	package Service_Delegator_Maps is new Ada.Containers.Ordered_Maps(
				Key_Type	=> Unbounded_String,
				Element_Type	=> Service_Delegator_Access
			);


	--------------------------------------
	-- Component Initialization Trigger --
	--------------------------------------

	type Initialization_Trigger_Access is not null access procedure;

	package Initialization_Trigger_Vectors is new Ada.Containers.Vectors(
				Index_Type	=> Positive,
				Element_Type	=> Initialization_Trigger_Access
			);

	----------------
	-- Components --
	----------------

	type Component_Type is abstract tagged record
		Service_Delegators	: Service_Delegator_Maps.Map;
		-- where I look for my services..

		Default_Service		: Unbounded_String;
		-- default service to load

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
			Config		: in out KOW_Config.Config_File
		) is null;
	-- setup the component while starting up the server
	-- Config is an already initialized configuration file located at:
	-- 	kowview/component_name
	--
	-- override this procedure whenever you need to write your initialization code



	procedure Initialize( Component : in out Component_Type );
	-- initialize the request doind:
	-- 	1. call the abstract setup procedure
	-- 	2. runing all the initialization triggers


	function Locate_Resource(
			Component	: in Component_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String;

	procedure Register_Service_Delegator(
			Component	: in out Component_Type;
			Name		: in     String;
			Delegator	: in     Service_Delegator_Access
		);

	procedure Register_Service_Delegator(
			Component	: in out Component_Type;
			Name		: in     Unbounded_String;
			Delegator	: in     Service_Delegator_Access
		);
	-- register a new service delegator...
	-- the name of this delegator is going to be calculated from the 

	function Get_Service_Delegator(
			Component	: in Component_Type;
			Data		: in AWS.Status.Data
		) return Service_Delegator_Access;
	-- return the service delegator for this request..
	-- you should override this method in case you want only one service in your component 
		

	procedure Process_Json_Request(
			Component	: in out Component_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out KOW_Lib.Json.Object_Type
		);

	procedure Process_Custom_Request(
			Component	: in out Component_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		);
	-- this is where the request processing takes place..
	-- can be overriding for implementing default services and such


	function Get_Name( Component : in Component_Type'Class ) return String;

	-------------
	-- Modules --
	-------------

	-- TODO :: implement delegator for modules as well
	-- and move the module type to it's own package


	type Module_Type is abstract tagged record
		Module_ID	: Positive;
		-- a number to identify the module in this request/page


		ID_Count	: Natural := 0;
		-- count all the ids that have been generated for this module

		Component	: Component_Ptr;
	end record;

	-- a module is something that can be accessed anywhere inside the system.

	type Module_Instance_Access is not null access all Module_Type'Class;





	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File 
		) return Module_Type'Class is abstract;
	-- create a new module instance.
	-- depending on the service, the instance object can represent different things and can, or not, even me extended
	-- to implement additional functionality.
	-- A service can also have it's own state which can be saved in the session for later retrieval.



	procedure Initialize_Request(
			Module		: in out Module_Type;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		) is null;
	-- Initialize the processing of a request
	-- Called before anything has been build.
	-- If Is_Final = True than stop processing other modules and return Response
	-- Useful when handling secured modules or modules that require sending cookies

	procedure Process_Header(
			Module		: in out Module_Type;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is null;
	-- process header of the response.
	-- it's assumed that 

	procedure Process_Request(
			Module		: in out Module_Type;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is null;
	-- process the request for a module.
	-- sometimes is useful for a module only to be created and released - such as in a page counter module

	procedure Process_Footer(
			Module		: in out Module_Type;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is null;
	-- process some footer of the module
	-- useful when creating benchmar modules

	procedure Finalize_Request(
			Module		: in out Module_Type;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
		) is null;
	-- Finalize processing the request.
	-- Called when the process has been finalized


	-- Helper Module Methods that can be overriden if needed
	
	function Locate_Resource(
			Module		: in Module_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String;



	procedure Generate_HTML_ID(
				Module		: in out Module_Type;
				The_ID		:    out Unbounded_String
		);
	-- procedure used to generate a valid ID for HTML elements
	-- it's a helper procedure so the user can produce unique IDs for their pages easily


end KOW_View.Components;
