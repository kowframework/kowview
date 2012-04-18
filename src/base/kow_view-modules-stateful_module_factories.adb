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
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

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

package body KOW_View.Modules.Stateful_Module_Factories is

	--------------
	-- The Data --
	--------------


	function Get_Key(
				Status		: in Request_Status_Type;
				Virtual_Host	: in KOW_View.Virtual_Host_Name_Type;
				Module_Id	: in Positive
			) return String is
		ID_Str : constant String := Ada.Strings.Fixed.Trim( Positive'Image( Module_Id ), Ada.Strings.Both );
		Context : constant String := AWS.Status.URI( Status.Request );
	begin
		if KOW_View.Enable_Virtual_Host then
			return Module_Container_Key_Prefix & Context & "::" & Virtual_Host & "::" & ID_Str;
		else
			return Module_Container_Key_Prefix & Context & "::" & ID_Str;
		end if;
	end Get_Key;

	function Get(
			Status		: in Request_Status_Type;
			Virtual_Host	: in KOW_View.Virtual_Host_Name_Type;
			Module_Id	: in Positive
		) return Module_Type is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session( Status.Request );
	begin
		return Module_Data.Get( Session_ID, Get_Key( Status, Virtual_Host, Module_ID ) );
	end Get;
	
	procedure Set(
			Status		: in Request_Status_Type;
			Module		: in Module_Type
		) is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session( Status.Request );
	begin
		Module_Data.Set(
					SID	=> Session_Id,
					Key	=> Get_Key( Status, Module.Virtual_Host, Module.Id ),
					Value	=> Module
				);
	end Set;


	-----------------
	-- The Factory --
	-----------------


	overriding
	procedure Create(
				Delegator	: in out Module_Factory_Type;
				Status		: in     Request_Status_Type;
				Module_Id	: in     Positive;
				Request_Mode	: in     Request_Mode_Type;
				Virtual_Host	: in     KOW_View.Virtual_Host_Name_Type;
				Module		:    out Module_Ptr
			) is
		-- create a module, setting it's ID if necessary
		
		The_Module : Module_Type_Access := new Module_Type'( Get( Status, Virtual_Host, Module_ID ) );
	begin
		The_Module.ID := Module_id;
		The_Module.ID_Count := 0;
		The_Module.Component := Component_Ptr( Component );
		The_Module.Request_Mode := Request_Mode;
		The_Module.Virtual_Host := Virtual_Host;

		Module := Module_Ptr( The_Module );
	end Create;

	overriding
	procedure Destroy(
				Delegator	: in out Module_Factory_Type;
				Status		: in     Request_Status_Type;
				Module		: in out Module_Ptr
			) is
		-- free the module access type
	begin
		Set( Status, Module_Type( Module.all ) );
		Free( Module_Type_Access( Module ) );
	end Destroy;
begin

	KOW_View.Components.Register_Module_Factory(
				Component	=> Component.all,
				Name		=> From_String( KOW_View.Modules.Util.Get_Name( Module_Type'Tag ) ),
				Factory		=> Factory_Instance'Access
			);

end KOW_View.Modules.Stateful_Module_Factories;
