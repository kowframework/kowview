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




-----------
-- Ahven --
-----------
with Ahven.Framework;
with Ahven.Text_Runner;

-----------
-- Tests --
-----------
with KOW_View_Tests;			use KOW_View_Tests;
with KOW_View_Tests.Components.Util;
with KOW_View_Tests.Modules.Util;
with KOW_View_Tests.Services.Util;
with KOW_View_Tests.URI_Util;
with KOW_View_Tests.Util;



procedure Run_Tests is
begin
	Suite := Ahven.Framework.Create_Suite( "KOW View Tests" );

	Ahven.Framework.Add_Test( Suite.all, new KOW_View_Tests.Components.Util.Test_Type );
	Ahven.Framework.Add_Test( Suite.all, new KOW_View_Tests.Modules.Util.Test_Type );
	Ahven.Framework.Add_Test( Suite.all, new KOW_View_Tests.Services.Util.Test_Type );
	Ahven.Framework.Add_Test( Suite.all, new KOW_View_Tests.URI_Util.Test_Type );
	Ahven.Framework.Add_Test( Suite.all, new KOW_View_Tests.Util.Test_Type );

	Ahven.Text_Runner.Run( KOW_View_Tests.Suite );
	Ahven.Framework.Release_Suite( KOW_View_Tests.Suite );
end Run_Tests;
