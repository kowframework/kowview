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


---------
-- AWS --
---------
with AWS.Status;

---------
-- Ada --
---------
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Generic_Registry;
with KOW_View.Components;		use KOW_View.Components;



package KOW_View.Components.Registry is

	----------------
	-- Exceptions --
	----------------


	DUPLICATED_COMPONENT_ERROR	: Exception;
	UNKNOWN_COMPONENT_ERROR		: Exception;


	--------------------------
	-- Component Management --
	--------------------------

	procedure Register(
				Component		: in KOW_View.Components.Component_Access;
				Require_Configuration	: in Boolean
			);
	-- A component, once registered, is never deallocated.
	-- All components should be registered at startup.
	--
	-- This procedure also calls "Initialize" for each component. Is also responsible for locating
	-- the component's configuration file.
	--
	-- If Require_Configuration == true and there is no config file available raise 
	-- COMPONENT_ERROR



	function Get_Component( Component_Name: in String ) return KOW_View.Components.Component_Access;
	-- get a component by it's name
	-- There is only one instance for each component.

	function Get_Component( Component_Name: in Component_Name_Type ) return KOW_View.Components.Component_Access;


	package Component_Maps is new Ada.Containers.Ordered_Maps(
					Key_Type	=> Component_Name_Type,
					Element_Type	=> Component_Ptr
				);


	function Get_Components return Component_Maps.Map;
	-- return all the components available;

private



	The_Registry: Component_Maps.Map;



end KOW_View.Components.Registry;
