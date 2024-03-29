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
with Ahven;
with Ahven.Framework;



-------------------
-- KOW Framework --
-------------------
with KOW_View;				use KOW_View;
with KOW_View.Components;
with KOW_View.Components.Util;		use KOW_View.Components.Util;

package body KOW_View_Tests.Components.Util is


	overriding
	procedure Initialize( T : in out Test_Type ) is
	begin
		Set_Name( T, "KOW_View.Components.Util" );
		Ahven.Framework.Add_Test_Routine( T, Test_Get_Name_Object'Access, "Get_Name( object )" );
		Ahven.Framework.Add_Test_Routine( T, Test_Get_Name_Tag'Access, "Get_Name( tag )" );
	end Initialize;



	procedure Test_Get_Name_Object is
		Expected_Name : constant String := "meu_componente";
		Computed_Name : constant String := To_String( KOW_View.Components.Get_Name( Component ) ); 
	begin
		Ahven.Assert(
				Condition	=> Expected_Name = Computed_Name,
				Message		=> Computed_Name & " is not valid (expected " & Expected_name & ")"
			);
	end Test_Get_Name_Object;

	procedure Test_Get_Name_Tag is
		Expected_Name : constant String := "meu_componente";
		Computed_Name : constant String := To_String( KOW_View.Components.Util.Get_Name( Meu_Componente_Component'Tag ) );
	begin
		Ahven.Assert(
				Condition	=> Expected_Name = Computed_Name,
				Message		=> Computed_Name & " is not valid (expected" & Expected_Name & ")"
			);
	end Test_Get_Name_Tag;

end KOW_View_Tests.Components.Util;
