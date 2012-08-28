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



-------------------
-- KOW Framework --
-------------------
with KOW_View.Pages;		use KOW_View.Pages;


package KOW_View.Modules.Factories is


	-----------------------
	-- Singleton Modules --
	-----------------------

	generic
		type Module_Type is new Module_Interface with private;
	package Singleton_Modules is

		type Singleton_Module_Factory is new Module_Factory_Interface with null record;
		-- allocate during package elaboration

		overriding
		procedure Create(
					Factory	: in out Singleton_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	:    out Module_Ptr
			);
	
		overriding
		procedure Destroy(
					Factory	: in out Singleton_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	: in out Module_Ptr
				);

		The_Module	: constant Module_Ptr := new Module_Type;
	end Singleton_Modules;



	----------------------
	-- Stateful Modules --
	----------------------

	generic
		type Module_Type is new Module_Interface with private;
		Session_Key : String;
		-- the key that will be used to store the service object
	package Stateful_Modules is

		type Stateful_Module_Factory is new Module_Factory_Interface with null record;
		-- allocate only once during the session, keeping it in the memory for the rest of the time
		-- avoid access types and unbounded_strings in modules that are to be stateful
		-- or you run into problems since the session stores the data in a stream
		--
		-- one exception to this rule is the component object, as it's always in the same memory region
		-- you shouldn't run into problems .
		--
		-- the module is always a copy of the actuall in-session module, not the actual instance

		overriding
		procedure Create(
					Factory	: in out Stateful_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	:    out Module_Ptr
				);

		overriding
		procedure Destroy(
					Factory	: in out Stateful_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	: in out Module_Ptr
				);
	end Stateful_Modules;


	-----------------------
	-- Stateless Modules --
	-----------------------

	generic
		type Module_Type is new Module_Interface with private;
	package Stateless_Modules is

		type Stateless_Module_Factory is new Module_Factory_Interface with null record;
		-- allocate everytime there is a request

		overriding
		procedure Create(
					Factory	: in out Stateless_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	:    out Module_Ptr
				);

		overriding
		procedure Destroy(
					Factory	: in out Stateless_Module_Factory;
					Status	: in     Request_Status_Type;
					Module	: in out Module_Ptr
				);
	end Stateless_Modules;


end KOW_View.Modules.Factories;
