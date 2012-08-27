
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

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.File_System;


package KOW_View.Themes is

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
			) is
		-- build the response for the given page
		Template_Path : constant String := Locate_Template(
								Theme_Engine	=> Theme_Engine_Type'Class( Theme_Engine ),
								Service		=> Service,
								Template	=> Template,
								Status		=> Status
							);
	begin

		Response : AWS.Response.Build(
						Content_Type	=> AWS.Mime.Text_HTML,
						Message_Body	=> KOW_View.KHTML.Render(
										File_Path	=> Template_Path,
										Initial_State	=> Initial_State
									)
							);
	end Build_Response;

	function Locate_Template(
				Theme_Engine	: in Theme_Engine_Type;
				Service		: in KOW_View.Services.Service_Type'Class;
				Template	: in Template_Name;
				Status		: in KOW_View.Request_Status_Type
			) return String is
		-- load the template, returning it as a String

		function Try( Str : in String ) return String is
		begin
			return KOW_View.Components.Locate_Resource(
						Component	=> Component.all,
						Resource	=> Str,
						Extension	=> Template_Extension,
						Status		=> Status
					);
		end Try;

		Tpl : constant String := To_String( Template );

		use KOW_Lib.File_System;
	begin
		return Try( To_String( KOW_View.Services.Get_Name( Service ) ) / Tpl );
	exception
		when Ada.Directories.Name_Error =>
			return Try( Tpl );
	end Locate_Template;


end KOW_View.Themes;
