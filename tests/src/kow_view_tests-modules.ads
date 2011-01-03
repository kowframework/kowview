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


-------------------
-- KOW Framework --
-------------------
with KOW_View.Modules;
with KOW_View.Modules.Stateless_Module_Factories;

with KOW_View_Tests.Components;


package KOW_View_Tests.Modules is
	type Meu_Modulo_Module is new KOW_View.Modules.Module_Type with null record;
	package Meu_Modulo_Factories is new KOW_View.Modules.Stateless_Module_Factories(
					Module_Type	=> Meu_Modulo_Module,
					Component	=> KOW_View_Tests.Components.Component'Access
				);

end KOW_View_Tests.Modules;
