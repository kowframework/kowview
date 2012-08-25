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
------------------------------------------------------------------------------
pragma License (GPL);

with ada.text_io;
--------------
-- Ada 2005 --
--------------
with Ada.Directories;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_View.KTML;
with KOW_View.Locales;
with KOW_View.Services;
with KOW_View.Services.Util;


---------
-- AWS --
---------
with AWS.Messages;
with AWS.MIME;
with AWS.Status;
with AWS.Response;

package body KOW_View.Services.Implementations is



	------------------
	-- KTML Service --
	------------------


	Resp_Att: constant String := "response";

	overriding
	procedure Process_Custom_Request(
				Service		: in out KTML_Service;
				Status		: in     Request_Status_Type;
				Response	:    out AWS.Response.Data
			) is
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

		Object : KOW_Lib.Json.Object_Type;
	begin
		Process_Json_Request(
					Service		=> KTML_Service'Class( Service ),
					Status		=> Status,
					Response	=> Object
				);

		if KOW_Lib.Json.Contains( Object, Resp_Att ) then
			Build_KTML_Response(
						Service		=> KTML_Service'Class( Service ),
						Status		=> Status,
						Template	=> KOW_Lib.Json.Get( Object, Resp_Att ),
						Initial_State	=> Object,
						Response	=> Response
					);
		else
			Build_KTML_Response(
						Service		=> KTML_Service'Class( Service ),
						Status		=> Status,
						Template	=> "success",
						Initial_State	=> Object,
						Response	=> Response
					);
		end if;

	end Process_Custom_Request;


	procedure Build_KTML_Response(
				Service		: in out KTML_Service;
				Status		: in     Request_Status_Type;
				Template	: in     String;
				Initial_State	: in     KOW_Lib.Json.Object_Type;
				Response	:    out AWS.Response.Data
			) is
		-- build the KTML response, being:
		-- 	template	: the name of the template being used, without the extension (assumed to be ktml)
		-- Localization is considered and the response is deflate encoded 

		Template_Path	: constant String := Locate_Resource(
								Service		=> KTML_Service'Class( Service ),
								Resource	=> Template,
								Extension	=> "ktml",
								Virtual_Host	=> KOW_View.Virtual_Host( Status ),
								Locale		=> KOW_View.Locales.Get_Locale( Status ) 
							);
	begin
		Response := AWS.Response.Build(
					Content_Type	=> "text/html",
					Message_Body	=> KOW_View.KTML.Render( Template_Path, Initial_State ),
					Encoding	=> AWS.Messages.Deflate
				);
	end Build_KTML_Response;

	procedure Set_Response(
				Service		: in out KTML_Service;
				Response	: in out KOW_Lib.Json.Object_Type;
				Value		: in     String
			) is
		-- set the response for the current request
	begin
		KOW_Lib.Json.Set( Response, Resp_Att, Value );
	end Set_Response;



	----------------------
	-- Resource Service --
	----------------------

	overriding
	procedure Process_Custom_Request(
				Service		: in out Resource_Service;
				Status		: in     Request_Status_Type;
				Response	:    out AWS.Response.Data
			) is
		-- serve a given file inside the service resource page
		Resource_URI	: constant String := To_String( Status.Local_URI );
		Locale		: constant KOW_Lib.Locales.Locale_Type := KOW_View.Locales.Get_Locale( Status );

		Extension	: constant String := Ada.Directories.Extension( Resource_URI );

		function resource return String is
		begin
			if Extension'Length > 0 then
				return Resource_URI( Resource_URI'First .. Resource_URI'Last - Extension'Length - 1);
			else
				return Resource_URI;
			end if;
		end resource;

		Resource_Path	: constant String := Locate_Resource(
								Service		=> Resource_Service'Class( Service ),
								Resource	=> Resource,
								Extension	=> Extension,
								Virtual_Host	=> KOW_View.Virtual_Host( Status ),
								Locale		=> Locale
							);
	begin
		-- we DO NOT encode JavaScript and CSS and anything else because... well... its not working at all
		-- the thing is, Amdjs files can't be encoded for some weird reason... as I have no time for
		-- this right now I'll leave this research for later on....
		--
		-- TODO :: see why Amdjs.js doesn't like gzip neither deflate
		Response := AWS.Response.File(
				Content_Type    => AWS.MIME.Content_Type( Resource_Path ),
				Filename        => Resource_Path
			);
	end Process_Custom_Request;


	overriding
	procedure Process_Json_Request(
				Service		: in out Resource_Service;
				Status		: in     Request_Status_Type;
				Response	:    out KOW_Lib.Json.Object_Type
			) is
	begin
		raise CONSTRAINT_ERROR with "I can't serve BLOB files in Json YET";
		-- TODO :: serve the files inside blob in json
	end Process_Json_Request;

end KOW_View.Services.Implementations;
