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
with AWS.Status;
with AWS.Response;

package KOW_View.Services.Implementations is



	type Resource_Service is new KOW_View.Services.Service_Type with null record;
	-- The resource service can be used by your own component to provide
	-- access to file resources, such as images and static HTML data.

	overriding
	procedure Process_Custom_Request(
				Service		: in out Resource_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out AWS.Response.Data
			);
	-- serve a given file inside the service resource page
			
	overriding
	procedure Process_Json_Request(
				Service		: in out Resource_Service;
				Request		: in     AWS.Status.Data;
				Response	:    out KOW_Lib.Json.Object_Type
			);

end KOW_View.Services.Implementations;
