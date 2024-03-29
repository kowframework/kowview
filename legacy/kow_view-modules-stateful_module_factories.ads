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
-- Factory for Stateful modules                                            --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Unchecked_Deallocation;

-------------------
-- KOW Framework --
-------------------
with KOW_View.Components;
with KOW_View.Modules;
with KOW_View.Modules.Util;

---------
-- AWS --
---------
with AWS.Status;
with AWS.Session;

generic
	type Module_Type is new KOW_View.Modules.Module_Type with private;
	Component : KOW_View.Components.Component_Access;
package KOW_View.Modules.Stateful_Module_Factories is
pragma Elaborate_Body( KOW_View.Modules.Stateful_Module_Factories );

	--------------
	-- The Data --
	--------------
	Module_Container_Key_Prefix : constant String := 
		To_String( KOW_View.Components.Get_Name( Component.all ) ) & "::" & To_String( KOW_View.Modules.Util.Get_Name( Module_Type'Tag ) ) & "::state::";
	
	package Values is
		Null_Module : Module_Type;
	end values;


	package Module_Data is new AWS.Session.Generic_Data(
				Data		=> Module_Type,
				Null_Data	=> Values.Null_Module
			);
	

	function Get_Key(
			Status		: in Request_Status_Type;
			Virtual_Host	: in KOW_View.Virtual_Host_Name_Type;
			Module_Id	: in Positive
		) return String;

	function Get(
			Status		: in Request_Status_Type;
			Virtual_Host	: in KOW_View.Virtual_Host_Name_Type;
			Module_Id	: in Positive
		) return Module_Type;
	
	procedure Set(
			Status		: in Request_Status_Type;
			Module		: in Module_Type
		);

	-----------------
	-- The Factory --
	-----------------


	type Module_Factory_Type is new Module_Factory_Interface with null record;


	overriding
	procedure Create(
				Delegator	: in out Module_Factory_Type;
				Status		: in     Request_Status_Type;
				Module_Id	: in     Positive;
				Request_Mode	: in     Request_Mode_Type;
				Virtual_Host	: in     KOW_View.Virtual_Host_Name_Type;
				Module		:    out Module_Ptr
			);
	-- create a module, setting it's ID if necessary

	overriding
	procedure Destroy(
				Delegator	: in out Module_Factory_Type;
				Status		: in     Request_Status_Type;
				Module		: in out Module_Ptr
			);
	-- free the module access type


	---------------
	-- Variables --
	---------------

	Factory_Instance : aliased Module_Factory_Type;

private
	----------
	-- Free --
	----------
	
	type Module_Type_Access is access all Module_Type;

	procedure Free is new Ada.Unchecked_Deallocation(
					Object	=> Module_Type,
					Name	=> Module_Type_Access
				);



end KOW_View.Modules.Stateful_Module_Factories;
