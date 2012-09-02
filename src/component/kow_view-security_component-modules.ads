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
with KOW_Lib.Json;
with KOW_View.Components;	use KOW_View.Components;
with KOW_View.Module_Factories;
with KOW_View.Modules;
with KOW_View.Pages;


package KOW_View.Security_Component.Modules is

	package Base is new KOW_View.Modules( KOW_View.Security_Component.Component );



	type Login_Controller_Module is new Base.KTML_Module with null record;
	-- renders the "login" or "logout" template


	overriding
	procedure Process_Json_Request(
				Module	: in out Login_Controller_Module;
				Page	: in out KOW_View.Pages.Page_Interface'Class;
				Status	: in     Request_Status_Type;
				Response:    out KOW_Lib.Json.Object_Type
			);
	package Login_Controller_Factories is new KOW_View.Module_Factories.Singleton_Modules( Login_Controller_Module );

	Login_Controller_Factory : KOW_View.Pages.Module_Factory_Ptr := new Login_Controller_Factories.Singleton_Module_Factory;

end KOW_View.Security_Component.Modules;
