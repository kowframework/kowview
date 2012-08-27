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

------------------------------------------------------------------------------
-- Main package for Theme Engines                                           --
------------------------------------------------------------------------------


-------------------
-- KOW Framework --
-------------------
with KOW_View.Components;
with KOW_View.Services;

package KOW_View.Themes is

	Component : constant KOW_View.Components.Component_Ptr := KOW_View.Components.New_Component( "themes" );

	subtype Template_Name is KOW_View.Path_Type;
	Template_Extension : constant String := "ktml";


	----------------------
	-- The Theme Engine --
	----------------------

	
	type Theme_Engine_Type is tagged null record;
	-- the theme is used by the page services
	-- this is the main theme implementation, but that can be overriden

	type Theme_Engine_Ptr is access all Theme_Engine_Type'Class;



	procedure Build_Response(
				Theme_Engine	: in     Theme_Engine_Type;
				Service		: in     KOW_View.Services.Service_Type'Class;
				Status		: in     KOW_View.Request_Status_Type;
				Template	: in     Template_Name;
				Initial_State	: in     KOW_Lib.Json.Object_Type;
				Response	:    out AWS.Response.Data
			);
	-- build the response for the given page


	function Locate_Template(
				Theme_Engine	: in Theme_Engine_Type;
				Service		: in KOW_View.Services.Service_Type'Class;
				Template	: in Template_Name;
				Status		: in KOW_View.Request_Status_Type
			) return String;
	-- load the template, returning it as a String


	function Locate_Theme_Resource(
				Theme_Engine	: in Theme_Engine_Type;
				Service		: in KOW_View.Services.Service_Type'Class;
				Resourc


	Default : constant Theme_Engine_Ptr := new Theme_Engine_Type;
	-- the default theme engine
	
end KOW_View.Themes;
