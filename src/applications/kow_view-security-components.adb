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


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;



-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Sec.Accounting;
with KOW_View.Components;
with KOW_View.URI_Util;				use KOW_View.URI_Util;


package body KOW_View.Security.Components is

	overriding
	procedure Setup(
			Component	: in out Security_Component;
			Config		: in out KOW_Config.Config_File
		) is
		use KOW_Config;

		My_Action : KOW_Sec.Accounting.Base_Action_Type'Class := KOW_Sec.Accounting.New_Action(
										Name		=> "setup",
										Root_Accountant	=> Accountant'Access
									);

		Default_Redirect	: constant String := Value( Config, "default_redirect", "" );
		Access_Denied		: constant String := Element( Config, "access_denied" );
		Login_Required		: constant String := Element( Config, "login_required" );


		function URI( N : in String ) return Unbounded_String is
		begin
			if Is_Page_URN( N ) then
				return To_Unbounded_String( To_Page_URI( N ) );
			else
				return To_Unbounded_String( N );
			end if;
		end URI;
	begin
		Default_Redirect_URI	:= URI( Default_Redirect );
		Access_Denied_URI	:= URI( Access_Denied );
		Login_Required_URI	:= URI( Login_Required );

		KOW_Sec.Accounting.Set_Exit_Status(
					My_Action,
					KOW_Sec.Accounting.Exit_Success,
					"security component setup completed"
				);
	end Setup;

end KOW_View.Security.Components;
