
G------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
-- Main package for Theme Engines                                           --
------------------------------------------------------------------------------



package KOW_View.Themes is


	----------------------
	-- The Theme Engine --
	----------------------

	type Theme_Engine_Type is interface;
	-- the theme is used by the page services

	type Theme_Engine_Ptr is access all Theme_Engine_Type'Class;



	function Load_Template(
				Theme_Engine	: in Theme_Engine_Type;
				Service		: in KOW_View.Services.Service_Type'Class;
				Template_Name	: in String;
				Status		: in KOW_Ent.Request_Status_Type
			) return String;
	-- load the template, returning it as a String



	
end KOW_View.Themes;
