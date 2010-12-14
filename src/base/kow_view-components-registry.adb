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


---------
-- Ada --
---------
with Ada.IO_Exceptions;
with Ada.Directories;			use Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Generic_Registry;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
with KOW_Lib.UString_Vectors;
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Components.Util;
with KOW_View.Util;



package body KOW_View.Components.Registry is


	--------------------------
	-- Component Management --
	--------------------------

	procedure Register(
				Component		: in KOW_View.Components.Component_Access;
				Require_Configuration	: in Boolean
			) is

		Component_name	: constant Unbounded_String := KOW_View.Util.Get_Type_name( Component.all'Tag, "_component" );

		use Component_Maps;
	begin
		-- the component is in the memory and is initialized:
		if Contains( The_Registry, Component_Name ) then
			raise DUPLICATED_COMPONENT_ERROR with To_String( Component_Name ) & "@" & Ada.Tags.Expanded_Name( Component.all'Tag );
		end if;

		Initialize( Component.all, Require_Configuration );

		Include( The_Registry, Component_Name, Component );
		
	end Register;



	function Get_Component( Component_Name: in String ) return KOW_View.Components.Component_Access is
		-- Loads a component by it's name
		-- There is only one instance for each component.
	begin
		return Get_Component( To_Unbounded_String( Component_Name ) );
	end Get_Component;


	function Get_Component( Component_Name: in Unbounded_String ) return KOW_View.Components.Component_Access is
	begin
		declare
			Component : Component_Access := Component_Maps.Element( The_Registry, Component_Name );
		begin
			return Component;
		end;
	exception
		when CONSTRAINT_ERROR =>
			raise UNKNOWN_COMPONENT_ERROR with To_String( Component_Name );
	end Get_Component;

	

	function Get_Component( Request : in AWS.Status.Data ) return KOW_View.Components.Component_Access is
		-- get the component for the given request.

		URI : constant String := AWS.Status.URI( Request );
	begin
		if URI'Length = 1 then -- URI = '/'
			raise KOW_View.REDIRECT_TO_HOME;
		else
			return Get_Component( KOW_View.Components.Util.Get_Name( AWS.Status.URI( Request ) ) );
		end if;
	end Get_Component;

	-----------------------
	-- Module Management --
	-----------------------

	function Load_Module(
			Component_Name	: in Unbounded_String;
			Module_Name	: in Unbounded_String;
			Config		: in KOW_Config.Config_File;
			Module_ID	: in Positive := 1
		) return Module_Type'Class is
		-- get a module instance
		Component	: Component_Access := Get_Component( Component_Name );
		Module		: Module_Type'Class := Create_Instance( Component.all, To_String( Module_Name ), Config );
	begin
		Module.Module_ID := Module_ID;
		Module.Component := Component_Ptr( Component );

		return Module;
	end Load_Module;


	function Load_Module(
			Component_Name	: in String;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File;
			Module_ID	: in Positive := 1
		) return Module_Type'Class is
		-- get a module instance
		Component	: Component_Access := Get_Component( Component_Name );
		Module		: Module_Type'Class := Create_Instance( Component.all, Module_Name, Config  );
	begin
		Module.Module_ID := Module_ID;
		Module.Component := Component_Ptr( Component );

		return Module;
	end Load_Module;


	function Load_Module(
			Component_Name	: in String;
			Module_Name	: in String;
			Module_ID	: in Positive := 1
		) return Module_Type'Class is
		-- get the module, using the standard module configuration
	begin
		return Load_Module(
				Component_Name	=> Component_Name,
				Module_Name	=> Module_Name,
				Config		=> KOW_View.Components.Util.Load_Configuration(
							Component_Name		=> Component_Name,
							Configuration_Name	=> Module_Name
						),
				Module_ID	=> Module_ID
				);
	end Load_Module;



end KOW_View.Components.Registry;
