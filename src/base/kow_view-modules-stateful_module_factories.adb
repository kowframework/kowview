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
				Context		: in String;
				Module_Id	: in Positive
			) return String is
		ID_Str : constant String := Ada.Strings.Fixed.Trim( Positive'Image( Module_Id ), Ada.Strings.Both );
	begin
		return Module_Container_Key_Prefix & Context & "::" & ID_Str;
	end Get_Key;

	function Get(
			Request		: in AWS.Status.Data;
			Context		: in String;
			Module_Id	: in Positive
		) return Module_Type is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		return Module_Data.Get( Session_ID, Get_Key( Context, Module_ID ) );
	end Get;
	
	procedure Set(
			Request	: in AWS.Status.Data;
			Module	: in Module_Type
		) is
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		Module_Data.Set(
					SID	=> Session_Id,
					Key	=> Get_Key( To_String( Module.Context ), Module.Id ),
					Value	=> Module
				);
	end Set;


	-----------------
	-- The Factory --
	-----------------


	overriding
	procedure Create(
				Delegator	: in out Module_Factory_Type;
				Request		: in     AWS.Status.Data;
				Context		: in     String;
				Module_Id	: in     Positive;
				Module		:    out Module_Ptr
			) is
		-- create a module, setting it's ID if necessary
		
		The_Module : Module_Type_Access := new Module_Type'( Get( Request, Context, Module_ID ) );
	begin
		The_Module.Context := Ada.Strings.Unbounded.To_Unbounded_String( Context );
		The_Module.ID := Module_id;
		The_Module.ID_Count := 0;
		The_Module.Component := Component_Ptr( Component );

		Module := Module_Ptr( The_Module );
	end Create;

	overriding
	procedure Destroy(
				Delegator	: in out Module_Factory_Type;
				Request		: in     AWS.Status.Data;
				Module		: in out Module_Ptr
			) is
		-- free the module access type
	begin
		Set( Request, Module_Type( Module.all ) );
		Free( Module_Type_Access( Module ) );
	end Destroy;
begin

	KOW_View.Components.Register_Module_Factory(
				Component	=> Component.all,
				Name		=> To_Unbounded_String( KOW_View.Modules.Util.Get_Name( Module_Type'Tag ) ),
				Factory		=> Factory_Instance'Access
			);

end KOW_View.Modules.Stateful_Module_Factories;
