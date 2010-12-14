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
with KOW_Lib.Json;
with KOW_View.Components;		use KOW_View.Components;


---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;

package KOW_View.Services is

	--------------
	-- Services --
	--------------


	type Service_Type is abstract tagged record
		-- this is the unit that process user requests
		-- even the page processing is implemented as a service
		--
		-- it's mapped as 

		Component	: Component_Ptr;
		-- the service to whom this service belongs

	end record;



	procedure Setup_Service(
			Service		: in out Service_Type;
			Config		: in     KOW_Config.Config_File
		) is null;
	-- for those services that can be configured, this is where you implement the configuration calling
	-- this is usefull if you have two diferent instances of the same service, each one with
	-- different setup...
	--
	-- this is new on kowview 2.0


	procedure Process_Json_Request(
			Service	: in out Service_Type;
			Request	: in     AWS.Status.Data;
			Response:    out KOW_Lib.Json.Object_Type
		) is abstract;

	procedure Process_Custom_Request(
			Service		: in out Service_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		) is abstract;
	-- process a request to a service
	-- the entire request is handled by the service
	-- sometimes is useful for a service only to be created and released - such as in a counter service



	function Locate_Resource(
			Service		: in Service_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String;

	procedure Setup_Service(
			Component	: in out Component_Access;
			Service		: in out Service_Type'Class
		);
	-- load the configuration file and run setup..



	function Get_Name( Service_Tag : in Ada.Tags.Tag ) return String;
	function Get_Name( Service : in Service_Type'Class ) return String;


end KOW_View.Services;
