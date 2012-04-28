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
with KOW_View;
with KOW_View.Util;		use KOW_View.Util;

package body KOW_View_Tests.Util is


	overriding
	procedure Initialize( T : in out Test_Type ) is
	begin
		Set_Name( T, "KOW_View.Util" );
		Ahven.Framework.Add_Test_Routine( T, Test_Get_Type_Name'Access, "Get_Type_Name" );
	end Initialize;


	procedure Test_Get_Type_Name is
		type Meu_Tipo_Type is tagged null record;

		Expected_Name : constant String := "meu_tipo";
		Computed_Name : constant String := KOW_View.To_String( Get_Type_Name( Meu_Tipo_Type'Tag ) );
	begin
		Ahven.Assert(
				Condition	=> Expected_Name = Computed_Name,
				Message		=> Computed_Name & " is not valid (expected " & Expected_name & ")"
			);
	end Test_Get_Type_Name;


end KOW_View_Tests.Util;
