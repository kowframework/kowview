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
-- Root package for KOW Sec integration in KOW View                         --
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- The security inside KOW View is managed by KOW Sec. It's  this in every  --
-- layer.                                                                   --
--                                                                          --
-- This is a higher level layer with some modules and services for handling --
-- some aspects of the application security, such as login                  --
------------------------------------------------------------------------------


-------------------
-- KOW Framework --
-------------------
with KOW_Sec.Accounting;
with KOW_View;

package KOW_View.Security is
	Accountant      : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "security", KOW_View.Accountant'Access );




end KOW_View.Security;
