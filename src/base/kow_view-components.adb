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


--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_View.Components.Registry;
with KOW_View.Components.Util;
with KOW_View.Util;



package body KOW_View.Components is



	---------------
	-- Component --
	---------------

	procedure Initialize(
			Component		: in out Component_Type;
			Require_Configuration	: in     Boolean
		) is

		Config : KOW_Config.Config_File_Type;

		procedure Trigger_Iterator( C : in Initialization_Trigger_Vectors.Cursor ) is
		begin
			Initialization_Trigger_Vectors.Element( C ).all;
		end Trigger_Iterator;
	begin
		begin
			Config := KOW_View.Components.Util.Load_Main_Configuration( Get_name( Component ) );
		exception
			when KOW_Config.File_Not_Found =>
				if Require_Configuration then
					raise COMPONENT_ERROR with "Missing required configuration for component " & Get_Name( Component );
				end if;
		end;
		Setup( Component_Type'Class( Component ), Config );
		
		Initialization_Trigger_Vectors.Iterate( Component.Initialization_Triggers, Trigger_Iterator'Access );
	end Initialize;

	function Locate_Resource(
			Component	: in Component_Type;
			Resource	: in String;
			Extension	: in String := "";
			Virtual_Host	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return String is
	begin
		return KOW_View.Components.Util.Locate_Resource(
					Component_Name	=> Get_Name( Component ),
					Resource	=> Resource,
					Extension	=> Extension,
					Virtual_Host	=> Virtual_Host,
					Kind		=> Kind,
					Locale_Code	=> Locale.Code
				);

	end Locate_Resource;


	procedure Register_Service_Delegator(
			Component	: in out Component_Type;
			Name		: in     String;
			Delegator	: in     Service_Delegator_Access
		) is
		Service_Name : Service_Name_Type := From_String( Name );
	begin
		Register_Service_Delegator(
				Component	=> Component,
				Name		=> Service_Name,
				Delegator	=> Delegator
			);
	end Register_Service_Delegator;


	procedure Register_Service_Delegator(
			Component	: in out Component_Type;
			Name		: in     Service_Name_Type;
			Delegator	: in     Service_Delegator_Access
		) is
		-- register a new service delegator...
		-- the name of this delegator is going to be calculated from the 
		use Service_Delegator_Maps;
	begin
		if Contains( Component.Service_Delegators, Name ) then
			raise CONSTRAINT_ERROR with "duplicated service :: " & To_String( Name ) & "@" & Get_Name( Component );
		end if;

		Include( Component.Service_Delegators, Name, Service_Delegator_Ptr( Delegator ) );
	end Register_Service_Delegator;



	function Get_Service_Delegator(
			Component	: in Component_Type;
			Service		: in Service_Name_Type
		) return Service_Delegator_Access is
		-- return the service delegator for this request..
		-- you should override this method in case you want only one service in your component 
		
		Deleg		: Service_Delegator_Ptr;


	begin
		if Service = No_Service then
			pragma Assert( Component.Default_Service /= null, "there is no default service in the component " & Get_Name( Component ) );
			return Service_Delegator_Access( Component.Default_Service );
		elsif Service_Delegator_Maps.Contains( Component.Service_Delegators, Service ) then
			Deleg := Service_Delegator_Maps.Element(
							Component.Service_Delegators,
							Service
						);
			if Deleg = null then
				raise SERVICE_ERROR with "unknown service (null): " & To_String( Service );
			else
				return Service_Delegator_Access( Deleg );
			end if;
		else
			raise SERVICE_ERROR with "unknown service (not registered): " & To_String( Service );
		end if;
	end Get_Service_Delegator;


	procedure Process_Json_Request(
			Component	: in out Component_Type;
			Status		: in     Request_Status_Type;
			Response	:    out KOW_Lib.Json.Object_Type
		) is
	begin
		Process_Json_Request(
				Delegator	=> Get_Service_Delegator( Component, Status.Service ).all,
				Status		=> Status,
				Response	=> Response
			);
	end Process_Json_Request;

	procedure Process_Custom_Request(
			Component	: in out Component_Type;
			Status		: in     Request_Status_Type;
			Response	:    out AWS.Response.Data
		) is
		-- this is where the request processing takes place..
		-- can be overriding for implementing default services and such
	begin
		Process_Custom_Request(
				Delegator	=> Get_Service_Delegator( Component, Status.Service ).all,
				Status		=> Status,
				Response	=> Response
			);
	end Process_Custom_Request;




	procedure Register_Module_Factory(
			Component	: in out Component_Type;
			Name		: in     Module_Name_Type;
			Factory		: in     Module_Factory_Access
		) is
		use Module_Factory_Maps;
	begin
		if Contains( Component.Module_Factories, Name ) then
			raise CONSTRAINT_ERROR with "duplicated module :: " & To_String( Name ) & "@" & Get_Name( Component );
		end if;

		Include( Component.Module_Factories, Name, Module_Factory_Ptr( Factory ) );
	end Register_Module_Factory;
	
	function Get_Module_Factory(
			Component	: in Component_Type;
			Name		: in Module_Name_Type
		) return Module_Factory_Access is
	begin
		return Module_Factory_Access( Module_Factory_Maps.Element( Component.Module_Factories, Name ) );
	exception
		when CONSTRAINT_ERROR =>
			raise CONSTRAINT_ERROR with "module " & To_String( Name ) & " not found at component " & Get_Name( Component );
	end Get_Module_Factory;




	procedure Register_Initialization_Trigger(
				Component		: in out Component_Type;
				Initialization_Trigger	: in     Initialization_Trigger_Access
			) is

		Tr : Initialization_Trigger_Ptr := Initialization_Trigger_Ptr( Initialization_Trigger );
	begin
		if Initialization_Trigger_Vectors.Contains( Component.Initialization_Triggers, Tr ) then
			raise CONSTRAINT_ERROR with "duplicated trigger detected at component " & Get_Name( Component );
		else
			Initialization_Trigger_Vectors.Append( Component.Initialization_Triggers, Tr );
		end if;
	end Register_Initialization_Trigger;




	function Get_Name( Component : in Component_Type'Class ) return String is
	begin
		return KOW_View.Components.Util.Get_Name( Component'Tag );
	end Get_Name;





end KOW_View.Components;
