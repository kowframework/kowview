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

		Config : KOW_Config.Config_File;

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
		Setup( Component, Config );
		
		Initialization_Trigger_Vectors.Iterate( Component.Initialization_Triggers, Trigger_Iterator'Access );
	end Initialize;

	function Locate_Resource(
			Component	: in Component_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return KOW_View.Components.Util.Locate_Resource(
					Component_Name	=> Get_Name( Component ),
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);

	end Locate_Resource;


	procedure Register_Service_Delegator(
			Component	: in out Component_Type;
			Name		: in     String;
			Delegator	: in     Service_Delegator_Access
		) is
	begin
		Register_Service_Delegator(
				Component	=> Component,
				Name		=> To_Unbounded_String( Name ),
				Delegator	=> Delegator
			);
	end Register_Service_Delegator;


	procedure Register_Service_Delegator(
			Component	: in out Component_Type;
			Name		: in     Unbounded_String;
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
			Data		: in AWS.Status.Data
		) return Service_Delegator_Access is
		-- return the service delegator for this request..
		-- you should override this method in case you want only one service in your component 
		
		URI		: constant String := AWS.Status.URI( Data );
		Rest_Of_URI	: constant String := URI( Get_Name( Component )'Length + 2 .. URI'Last );
		Last		: Integer := Ada.Strings.Fixed.Index( Rest_of_Uri, "/" ) - 1;


		function Delegator( Name : Unbounded_String ) return Service_Delegator_Access is
		begin
			return Service_Delegator_Access(
				Service_Delegator_Maps.Element(
							Component.Service_Delegators,
							Name
						)
					);
		exception
			when CONSTRAINT_ERROR =>
				raise SERVICE_ERROR with "unknown service: " & To_String( Name );
		end Delegator;
	begin
		if Rest_of_uri'Length = 0 then
			pragma Assert( Component.Default_Service /= null, "there is no default component in the service " & Get_Name( Component ) );
			return Service_Delegator_Access( Component.Default_Service );
		end if;

		if Last < 0 then
			Last := Rest_of_Uri'Last;
		end if;

		return Delegator( To_Unbounded_String( Rest_of_Uri( Rest_of_Uri'First .. Last ) ) );
	end Get_Service_Delegator;


	procedure Process_Json_Request(
			Component	: in out Component_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out KOW_Lib.Json.Object_Type
		) is
	begin
		Process_Json_Request(
				Delegator	=> Get_Service_Delegator( Component, Request ).all,
				Request		=> Request,
				Response	=> Response
			);
	end Process_Json_Request;

	procedure Process_Custom_Request(
			Component	: in out Component_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		) is
		-- this is where the request processing takes place..
		-- can be overriding for implementing default services and such
	begin
		Process_Custom_Request(
				Delegator	=> Get_Service_Delegator( Component, Request ).all,
				Request		=> Request,
				Response	=> Response
			);
	end Process_Custom_Request;




	procedure Register_Module_Factory(
			Component	: in out Component_Type;
			Name		: in     Unbounded_String;
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
			Name		: in Unbounded_String
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
