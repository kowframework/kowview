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
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_View.Components_Registry;



package body KOW_View.Components is

	function Locate_Resource(
			Component	: in Component_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return KOW_View.Components_Registry.Locate_Resource(
					Component_Name	=> To_String( Component.Component_Name ),
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);

	end Locate_Resource;


	function Locate_Resource(
			Module		: in Module_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return Locate_Resource(
					Component	=> Module.Component.all,
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);
	end Locate_Resource;



	procedure Generate_HTML_ID(
				Module		: in out Module_Type;
				The_ID		:    out Unbounded_String
		) is
		-- procedure used to generate a valid ID for HTML elements
		-- it's a helper procedure so the user can produce unique IDs for their pages easily

		function T( N : in Natural ) return String is
			use Ada.Strings, Ada.Strings.Fixed;
		begin
			return Trim( Natural'Image( N ), Both );
		end T;

	begin
		Module.ID_Count := Module.ID_Count + 1;

		The_ID := To_Unbounded_String( "module_" & T( Natural( Module.Module_ID ) ) & "_id_" & T( Module.ID_Count ) );
				
	end Generate_HTML_ID;




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


end KOW_View.Components;
