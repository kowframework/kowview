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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.Json;
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Services.Util;
with KOW_View.Util;


---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;

package body KOW_View.Services is

	-------------
	-- Service --
	-------------

	function Locate_Resource(
			Service		: in Service_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return Locate_Resource(
					Component	=> Service.Component.all,
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);
	end Locate_Resource;


	procedure Setup_Service(
			Component	: in     Component_Access;
			Service		: in out Service_Type'Class
		) is
		-- load the configuration file and run setup..
	begin
		Service.Component := Component_Ptr( Component );
		declare
			use KOW_Config;
			use KOW_view.Util;
			Config : Config_File := New_Config_File(
							Get_Name( Component.all ) / Get_Name( Service )
						);
		begin
			Setup_Service( Service, Config );
		end;
	exception
		when KOW_Config.FILE_NOT_FOUND => null;
	end Setup_Service;



	function Get_Name( Service : in Service_Type'Class ) return String is
	begin
		return KOW_View.Services.Util.Get_Name( Service'Tag );
	end Get_Name;


end KOW_View.Services;
