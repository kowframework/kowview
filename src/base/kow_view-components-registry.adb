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


---------
-- Ada --
---------
with Ada.IO_Exceptions;
with Ada.Directories;			use Ada.Directories;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Generic_Registry;
with KOW_Lib.File_System;
with KOW_Lib.String_Util;
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

		Component_name	: constant Component_Name_Type := KOW_View.Util.Get_Type_name( Component.all'Tag, "_component" );

		use Component_Maps;
	begin
		-- the component is in the memory and is initialized:
		if Contains( The_Registry, Component_Name ) then
			raise DUPLICATED_COMPONENT_ERROR with To_String( Component_Name ) & "@" & Ada.Tags.Expanded_Name( Component.all'Tag );
		end if;

		Initialize( Component.all, Require_Configuration );

		Include( The_Registry, Component_Name, Component_Ptr( Component ) );
		
	end Register;



	function Get_Component( Component_Name: in String ) return KOW_View.Components.Component_Access is
		-- Loads a component by it's name
		-- There is only one instance for each component.
		C_Name : Component_Name_Type;
	begin
		KOW_Lib.String_Util.Copy( From => Component_Name, To => String( C_Name ) );
		return Get_Component( C_Name );
	end Get_Component;


	function Get_Component( Component_Name: in Component_Name_Type ) return KOW_View.Components.Component_Access is
	begin
		declare
			Component : Component_Access := Component_Access( Component_Maps.Element( The_Registry, Component_Name ) );
		begin
			return Component;
		end;
	exception
		when CONSTRAINT_ERROR =>
			raise UNKNOWN_COMPONENT_ERROR with To_String( Component_Name );
	end Get_Component;

	
	function Get_Components return Component_Maps.Map is
		-- return all the components available;
	begin
		return The_Registry;
	end Get_Components;

end KOW_View.Components.Registry;
