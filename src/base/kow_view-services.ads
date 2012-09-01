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



--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Finalization;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_View.Components;				use KOW_View.Components;
with KOW_View.Request_Dispatchers.Implementations;


---------
-- AWS --
---------
with AWS.Response;

package KOW_View.Services is


	------------------
	-- Service Type --
	------------------


	--type Service_Type is abstract new Ada.Finalization.Controlled with record
	type Service_Type is abstract tagged record
		-- this is the unit that process user requests
		-- even the page processing is implemented as a service
		--
		-- it's mapped as 

		Component	: Component_Ptr;
		-- the service to whom this service belongs
	end record;

	type Service_Ptr is access all Service_Type'Class;


	procedure Process_Json_Request(
			Service	: in out Service_Type;
			Status	: in     Request_Status_Type;
			Response:    out KOW_Lib.Json.Object_Type
		) is abstract;

	procedure Process_Custom_Request(
			Service		: in out Service_Type;
			Status		: in     Request_Status_Type;
			Response	:    out AWS.Response.Data
		) is abstract;
	-- process a request to a service
	-- the entire request is handled by the service
	-- sometimes is useful for a service only to be created and released - such as in a counter service



	function Locate_Resource(
			Service		: in Service_Type;
			Status		: in Request_Status_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String;
	-- locate the resource [service_name]/resource.extension within the component


	function Load_Config(
			Service	: in Service_Type;
			N	: in String := "setup"
		) return KOW_Config.Config_File_Type;


	function Get_Name( Service : in Service_Type ) return Service_Name;
	-- return the name of the service
	-- the service type has to be named [SERVICE_NAME]_Service



	---------------------
	-- Service Factory --
	---------------------


	type Service_Factory_Interface is interface;
	-- The service factory controlls the life cycle of the services
	
	type Service_Factory_Ptr is access all Service_Factory_Interface'Class;

	procedure Create(
				Factory	: in out Service_Factory_Interface;
				Status	: in     Request_Status_Type;
				Service	:    out Service_Ptr
			) is abstract;
	-- allocate and return the service

	procedure Destroy(
				Factory	: in out Service_Factory_Interface;
				Status	: in     Request_Status_Type;
				Service	: in out Service_Ptr
			) is abstract;
	-- deallocate the service; the pointer to Service should be null


	-------------------------
	-- Service Dispatchers --
	-------------------------

	type Service_Dispatcher_Type is abstract new KOW_View.Request_Dispatchers.Implementations.Prefix_Dispatcher_Type with record
		Factory : Service_Factory_Ptr;
	end record;


	overriding
	function Dispatch(
				Dispatcher	: in Service_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return AWS.Response.Data;

end KOW_View.Services;
