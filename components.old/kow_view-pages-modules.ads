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
pragma License( GPL );


------------------------------------------------------------------------------
-- Main package for the Pages application                                   --
------------------------------------------------------------------------------

--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_View.Modules;
with KOW_View.Modules.Implementations;
with KOW_View.Modules.Stateless_Module_Factories;
with KOW_View.Pages.Components;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Status;

package KOW_View.Pages.Modules is


	---------------------------
	-- Exception Test Module --
	---------------------------

	AN_EXCEPTION : Exception;

	type Exception_Test_Module is new KOW_View.Modules.Module_Type with null record;
	
	overriding
	procedure Process_Body(
			Module	: in out Exception_Test_Module;
			Status	: in     Request_Status_Type;
			Response:    out Ada.Strings.Unbounded.Unbounded_String
		);

	package Exception_Test_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
					Module_Type	=> Exception_Test_Module,
					Component	=> KOW_View.Pages.Components.Component'Access
				);


	-------------------
	-- Static Module --
	-------------------

	type Static_Module is new KOW_View.Modules.Implementations.Resource_Module with null record;
	-- serve static content...

	package Static_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
					Module_Type	=> Static_Module,
					Component	=> KOW_View.Pages.Components.Component'Access
				);
	
	-----------------
	-- Void Module --
	-----------------

	type Void_Module is new KOW_View.Modules.Module_type with null record;

	package Void_Module_Factories is new KOW_View.Modules.Stateless_Module_Factories(
					Module_type	=> Void_Module,
					Component	=> KOW_View.Pages.Components.Component'Access
				);

end KOW_View.Pages.Modules;
