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
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-----------
-- Ahven --
-----------
with Ahven;
with Ahven.Framework;

-------------------
-- KOW Framework --
-------------------
with KOW_View.Components;
with KOW_View.Modules;
with KOW_View.Modules.Util;		use KOW_View.Modules.Util;


with KOW_View_Tests.Components;

---------
-- AWS --
---------
with AWS.Status;

package body KOW_View_Tests.Modules.Util is


	overriding
	procedure Initialize( T : in out Test_Type ) is
	begin
		Set_Name( T, "KOW_View.Modules.Util" );
		Ahven.Framework.Add_Test_Routine( T, Test_Create_Module'Access, "Create_Module" ); 
		Ahven.Framework.Add_Test_Routine( T, Test_Get_Name_Object'Access, "Get_Name( object )" );
		Ahven.Framework.Add_Test_Routine( T, Test_Get_Name_Tag'Access, "Get_Name( tag )" );
		Ahven.Framework.Add_Test_Routine( T, Test_Component'Access, "Component" );
	end Initialize;





	function Get_Module return Meu_Modulo_Module is
		use KOW_View.Components;
		Dumb_Request	: AWS.Status.Data;

		Module		: KOW_View.Components.Module_Ptr;
		Expected_Name : constant String := "meu_modulo";
	begin
		Create(
					Factory		=> KOW_View_Tests.Components.Get_Module_Factory(
									KOW_View_Tests.Components.Component,
									To_Unbounded_String( Expected_Name )
								).all,
					Request		=> Dumb_Request,
					Context		=> "/some/page",
					Module_Id	=> 1,
					Module		=> module
			);
		Ahven.Assert(
				Condition	=> Module /= null,
				Message		=> "Can not create module... returning null pointer for some reason"
			);

		return Meu_Modulo_Module( Module.all );
	end Get_Module;


	procedure Test_Create_Module is
		use KOW_View.Components;
		Dumb_Request	: AWS.Status.Data;

		Module		: KOW_View.Components.Module_Ptr;
		Expected_Name : constant String := "meu_modulo";
	begin
		Create(
					Factory		=> KOW_View_Tests.Components.Get_Module_Factory(
									KOW_View_Tests.Components.Component,
									To_Unbounded_String( Expected_Name )
								).all,
					Request		=> Dumb_Request,
					Context		=> "/some/page",
					Module_Id	=> 1,
					Module		=> module
			);
		Ahven.Assert(
				Condition	=> Module /= null,
				Message		=> "Can not create module... returning null pointer for some reason"
			);

		Ahven.Assert(
				Condition	=> Module.all in Meu_Modulo_Module'Class,
				Message		=> "Returned invalid module type"
			);
	
	end Test_Create_Module;

	procedure Test_Get_Name_Object is
		Module : Meu_Modulo_Module := Get_Module;
		Computed_Name : constant String := KOW_View.Modules.Get_Name( Module );
		Expected_Name : constant String := "meu_modulo";
	begin
		Ahven.Assert(
				Condition	=> Expected_Name = Computed_Name,
				Message		=> Computed_Name & " is not valid (expected " & Expected_name & ")"
			);
	end Test_Get_Name_Object;





	procedure Test_Get_Name_Tag is
		Expected_Name : constant String := "meu_modulo";
		Computed_Name : Constant String := KOW_View.Modules.Util.Get_Name( Meu_Modulo_Module'Tag );
	begin
		Ahven.Assert(
				Condition	=> Expected_Name = Computed_Name,
				Message		=> Computed_Name & " is not valid (expected" & Expected_Name & ")"
			);
	end Test_Get_Name_Tag;


	procedure Test_Component is
		use KOW_View.Components;
		Module : Meu_Modulo_Module := Get_Module;
	begin
		Ahven.Assert(
				Condition	=> Module.Component = KOW_View_Tests.Components.Component'Access,
				Message		=> "not pointing to the right component..."
			);
	end Test_Component;


end KOW_View_Tests.Modules.Util;
