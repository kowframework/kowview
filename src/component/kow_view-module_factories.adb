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
pragma License (GPL);



--------------
-- Ada 2005 --
--------------
with Ada.Unchecked_Deallocation;

---------
-- AWS --
---------
with AWS.Session;
with AWS.Status;


-------------------
-- KOW Framework --
-------------------
with KOW_View.Pages;		use KOW_View.Pages;


package body KOW_View.Module_Factories is

	-----------------------
	-- Singleton Modules --
	-----------------------

	package body Singleton_Modules is

		overriding
		procedure Create(
					Factory	: in out Singleton_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	:    out Module_Ptr
			) is
		begin
			Module := The_Module;
		end Create;
	
		overriding
		procedure Destroy(
					Factory	: in out Singleton_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	: in out Module_Ptr
				) is
		begin
			Module := null;
		end Destroy;
	end Singleton_Modules;


	----------------------
	-- Stateful Modules --
	----------------------

	package body Stateful_Modules is


		type Module_Access is access all Module_Type;

		procedure Free is new Ada.Unchecked_Deallocation( Object => Module_Type, Name => Module_Access );
		Null_Module : Module_Type;

		package Module_Container_Data is new AWS.Session.Generic_Data(
				Data		=> Module_Type,
				Null_Data	=> Null_Module 
			);

		overriding
		procedure Create(
					Factory	: in out Stateful_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	:    out Module_Ptr
				) is
			Session_ID : constant AWS.Session.ID := AWS.Status.Session( Status.Request );
			Module_Template	: Module_Type := Module_Container_Data.Get( Session_Id, Session_Key );
			The_Module	: Module_Access := new Module_Type'( Module_Template );
		begin
			Module := Module_Ptr( The_Module );
		end Create;

		overriding
		procedure Destroy(
					Factory	: in out Stateful_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	: in out Module_Ptr
				) is
			Session_ID : constant AWS.Session.ID := AWS.Status.Session( Status.Request );
		begin
			Module_Container_Data.Set( Session_ID, Session_Key, Module_Access( Module ).all );
			Free( Module_Access( Module ) );
		end Destroy;
	end Stateful_Modules;



	-----------------------
	-- Stateless Modules --
	-----------------------

	package body Stateless_Modules is

		type Module_Access is access all Module_Type;
		procedure Free is new Ada.Unchecked_Deallocation( Object => Module_Type, Name => Module_Access );

		overriding
		procedure Create(
					Factory	: in out Stateless_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	:    out Module_Ptr
				) is
			The_Module : Module_Access := new Module_Type;
		begin
			Module := Module_Ptr( The_Module );
		end Create;

		overriding
		procedure Destroy(
					Factory	: in out Stateless_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	: in out Module_Ptr
				) is
		begin
			Free( Module_Access( Module ) );
		end Destroy;

	end Stateless_Modules;





end KOW_View.Module_Factories;
