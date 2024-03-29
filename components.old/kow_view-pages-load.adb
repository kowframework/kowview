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
pragma License( GPL );



-------------------
-- KOW Framework --
-------------------
with KOW_View.Components.Registry;
with KOW_View.Pages;			use KOW_View.Pages;
with KOW_View.Pages.Components;

with KOW_View.Pages.Modules;
pragma Elaborate( KOW_View.Pages.Modules );
with KOW_View.Pages.Services;
pragma Elaborate( KOW_View.Pages.Services );


procedure KOW_View.Pages.Load is
begin
	KOW_View.Components.Registry.Register(
			Components.Component'Access,
			False
		);

	
end KOW_View.Pages.Load;
