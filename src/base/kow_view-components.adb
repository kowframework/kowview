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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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



	procedure Initialize( Component : in out Component_Type ) is
	begin
	end Initialize;

	function Locate_Resource(
			Component	: in Component_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return KOW_View.Components.Registry.Locate_Resource(
					Component_Name	=> To_String( Component.Component_Name ),
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);

	end Locate_Resource;



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
			raise CONSTRAINT_ERROR with "duplicated service :: " & To_String( Name );
		end if;

		Include( Component.Service_Delegators, Name, Delegator );
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
			return Service_Delegator_Maps.Element(
						Component.Service_Delegators,
						Name
					);
		exception
			when CONSTRAINT_ERROR =>
				raise SERVICE_ERROR with "unknown service: " & To_String( Name );
		end Delegator;
	begin
		if Rest_of_uri'Length = 0 then
			return Delegator( Component.Default_Service );
		if Last < 0 then
			Last := Res_of_Uri'Last;
		end if;

		return Delegator( To_Unbounded_String( Rest_of_Uri( Rest_of_Uri'First .. Last ) );
	end Get_Service_Delegator;


	procedure Process_Json_Request(
			Component	: in out Component_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out KOW_Lib.Json.Object_Type
		) is
	begin
		Process_Json_Request(
				Delegator	=> Get_Delegator( Component, Request ).all,
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
				Delegator	=> Get_Delegator( Component, Request ),
				Request		=> Request,
				Response	=> Response
			);
	end Process_Custom_Reques;


	function Get_Name( Component : in Component_Type'Class ) return String is
	begin
		return KOW_View.Components.Util.Get_Name( Component'Tag );
	end Get_Name;

	------------
	-- Module --
	------------


	function Locate_Resource(
			Module		: in Module_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return Locate_Resource(
					Component	=> Module.Component.all,
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);
	end Locate_Resource;



	procedure Generate_HTML_ID(
				Module		: in out Module_Type;
				The_ID		:    out Unbounded_String
		) is
		-- procedure used to generate a valid ID for HTML elements
		-- it's a helper procedure so the user can produce unique IDs for their pages easily

		function T( N : in Natural ) return String is
			use Ada.Strings, Ada.Strings.Fixed;
		begin
			return Trim( Natural'Image( N ), Both );
		end T;

	begin
		Module.ID_Count := Module.ID_Count + 1;

		The_ID := To_Unbounded_String( "module_" & T( Natural( Module.Module_ID ) ) & "_id_" & T( Module.ID_Count ) );
				
	end Generate_HTML_ID;






end KOW_View.Components;
