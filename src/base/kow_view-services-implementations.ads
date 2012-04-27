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
-- Provides some generic service implementations you can use in your own    --
-- components.                                                              --
------------------------------------------------------------------------------


-- The services implemented here must be extended in your package and then
-- a service cycle package must be instanciated for your own type...
-- 
-- This is how it's intended to be. :)



-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_View.Services;


---------
-- AWS --
---------
with AWS.Response;

package KOW_View.Services.Implementations is


	------------------
	-- KTML Service --
	------------------


	type KTML_Service is abstract new KOW_View.Services.Service_Type with null record;


	overriding
	procedure Process_Custom_Request(
				Service		: in out KTML_Service;
				Status		: in     Request_Status_Type;
				Response	:    out AWS.Response.Data
			);
	-- call's Process_Json_Request and process the:
	--
	-- The template used is by default the module resource "success.ktml". It means:
	--
	-- 	[module_name]_module/success.ktml
	-- template using the json response as the initial state for the KTML parser.
	--
	-- if The parameter "response" is set then uses the template [response_value].ktml, ie:
	-- 	[module_name]_module/[response_value].ktml
	--
	-- Localization is considered. :)
	--
	-- All the calls are dynamically dispatched



	procedure Build_KTML_Response(
				Service		: in out KTML_Service;
				Status		: in     Request_Status_Type;
				Template	: in     String;
				Initial_State	: in     KOW_Lib.Json.Object_Type;
				Response	:    out AWS.Response.Data
			);
	-- build the KTML response, being:
	-- 	template	: the name of the template being used, without the extension (assumed to be ktml)
	-- Localization is considered and the response is deflate encoded 


	procedure Set_Response(
				Service		: in out KTML_Service;
				Response	: in out KOW_Lib.Json.Object_Type;
				Value		: in     String
			);
	-- set the response for the current request


	----------------------
	-- Resource Service --
	----------------------

	type Resource_Service is abstract new KOW_View.Services.Service_Type with null record;
	-- The resource service can be used by your own component to provide
	-- access to file resources, such as images and static HTML data.

	overriding
	procedure Process_Custom_Request(
				Service		: in out Resource_Service;
				Status		: in     Request_Status_Type;
				Response	:    out AWS.Response.Data
			);
	-- serve a given file inside the service resource page
			
	overriding
	procedure Process_Json_Request(
				Service		: in out Resource_Service;
				Status		: in     Request_Status_Type;
				Response	:    out KOW_Lib.Json.Object_Type
			);

end KOW_View.Services.Implementations;
