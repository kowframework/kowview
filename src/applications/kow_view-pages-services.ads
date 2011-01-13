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
-- Main package for the Pages services                                      --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.Json;
with KOW_View.Components;
with KOW_View.Pages.Components;
with KOW_View.Services;
with KOW_View.Services.Implementations;
with KOW_View.Services.Stateless_Service_Cycles;
with KOW_View.Themes;


---------
-- AWS --
---------
with AWS.Status;
with AWS.Response;


package KOW_View.Pages.Services is


	-------------------------------
	-- Helper Types and Packages --
	-------------------------------

	type Complete_Module_Type is record
		Module	: KOW_View.Components.Module_Ptr;
		Factory	: KOW_View.Components.Module_Factory_Ptr;
		Config	: KOW_Config.Config_File;
	end record;

	type Complete_Module_Array is array( Positive range <> ) of Complete_Module_Type;

	type Index_Array is array( Natural range <> ) of Positive;

	----------------------
	-- The Page Service --
	----------------------

	type Page_Service is new KOW_View.Services.Service_Type with record
		Page_Title	: Unbounded_String;
		Author		: Unbounded_String;
	end record;


	overriding
	procedure Process_Json_Request(
			Service	: in out Page_Service;
			Request	: in     AWS.Status.Data;
			Response:    out KOW_Lib.Json.Object_Type
		);
	-- run initialize for each one of the modules in the page
	-- then call Process_Json_Request for a given module or group of modules.

	overriding
	procedure Process_Custom_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		);
	-- process the entire module cycle returning a HTML page


	procedure Process_Custom_Request(
			Service		: in out Page_Service;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data;
			Page		: in     String;
			Initialize_Only	: in     Boolean
		);
	-- this is the actual procedure for processing the page and is here for
	-- the navigation component
	-- if initialize_only = true don't process head, body or foot...
	-- only tries to run initialize and then finalize..
	--
	-- IF any exception is raised during any of the procedures, simply jump to destroy
	-- all modules..

	function Get_Page(
				Service	: in Page_Service;
				Request	: in AWS.Status.Data
			) return String;
	-- retrieve the page name :)


	procedure Setup(
				Service	: in out Page_Service;
				Config	: in out KOW_Config.Config_File
			);


	package Page_Service_Cycles is new KOW_View.Services.Stateless_Service_Cycles(
						Service_Type	=> Page_Service,
						Component	=> KOW_View.Pages.Components.Component'Access
					);

	------------------------
	-- The Static Service --
	------------------------

	type Static_Service is new KOW_View.Services.Implementations.Resource_Service with null record;
	package Static_Service_Cycles is new KOW_View.Services.Stateless_Service_Cycles(
						Service_Type	=> Static_Service,
						Component	=> KOW_View.Pages.Components.Component'Access
					);



end KOW_View.Pages.Services;