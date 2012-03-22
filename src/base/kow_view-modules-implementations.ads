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
-- Some basic module implementations for KOW View                           --
------------------------------------------------------------------------------



-- To use these modules simply extend them in your project and instanciate
-- a module factory as usual.



--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_View.Modules;

---------
-- AWS --
---------
with AWS.Status;

package KOW_View.Modules.Implementations is

	type Resource_Module is new KOW_View.Modules.Module_Type with record
		-- used to read file system resources as text, returning it
		Resource : Unbounded_String;
	end record;


	overriding
	procedure Initialize_Request(
			Module		: in out Resource_Module;
			Request		: in     AWS.Status.Data;
			Config		: in out KOW_Config.Config_File_Type
		);

	overriding
	procedure Process_Body(
				Module	: in out Resource_Module;
				Request	: in     AWS.Status.Data;
				Response:    out Unbounded_String
			);
	-- return the content specified by the configuration
end KOW_View.Modules.Implementations;
