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


--------------
-- Ada 2005 --
--------------
with Ada.Exceptions;



-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;			use KOW_Lib.Json;
with KOW_Lib.String_Util;
with KOW_View.URI_Util;

---------
-- AWS --
---------
with AWS.Messages;
with AWS.Response;

package body KOW_View.json_util is


	function Build_Error_Response(
			E		: Ada.Exceptions.Exception_Occurrence;
			Status_Code	: AWS.Messages.Status_Code := AWS.Messages.S505;
			Cache_Control	: AWS.Messages.Cache_Option := AWS.Messages.No_Cache;
			Wrap_Data	: Boolean := False
		) return AWS.Response.Data is
		
		use Ada.Exceptions;
		use KOW_Lib.String_Util;


		Object	: Object_Type;
	begin
		Set( Object, "status", "error" );
		Set( Object, "error", Exception_Name( e ) );
		Set( Object, "message", Exception_Message( e ) );


		return Build(
				Message_Body	=> To_Json( Object ),
				Status_Code	=> Status_Code,
				Cache_Control	=> Cache_Control,
				Wrap_Data	=> Wrap_Data
			);
	end Build_Error_Response;


	
	function Build_Success_Response(
			Object		: KOW_Lib.Json.Object_Type;
			Status_Code	: AWS.Messages.Status_Code := AWS.Messages.S200;
			Cache_Control	: AWS.Messages.Cache_Option := AWS.Messages.No_Cache;
			Wrap_Data	: Boolean := False
		) return AWS.Response.Data is

		Response : Object_Type;
	begin

		KOW_Lib.Json.Set(
				Object	=> Response,
				Key	=> "status",
				Value	=> "success"
			);
		KOW_Lib.Json.Set(
				Object	=> Response,
				Key	=> "response",
				Value	=> Object 
			);
		return Build(
				Message_Body	=> To_Json( Response ),
				Status_Code	=> Status_Code,
				Cache_Control	=> Cache_Control,
				Wrap_Data	=> Wrap_Data
			);
	end Build_Success_Response;


	function Build_Redirect_Response(
			URI		: String;
			Status_Code	: AWS.Messages.Status_Code := AWS.Messages.S200;
			Cache_Control	: AWS.Messages.Cache_Option := AWS.Messages.No_Cache;
			Wrap_Data	: Boolean := False
		) return AWS.Response.Data is
		use KOW_View.URI_Util;
		Response : Object_Type;
	begin
		KOW_Lib.Json.Set(
				Object	=> Response,
				Key	=> "status",
				Value	=> "redirect"
			);

		if Is_Page_URN( URI ) then
			KOW_Lib.Json.Set(
					Object	=> Response,
					Key	=> "to",
					Value	=> To_Page_URI( URI )
				);
		else
			KOW_Lib.Json.Set(
					Object	=> Response,
					Key	=> "to",
					Value	=> URI
				);
		end if;

		return Build(
				Message_Body	=> To_Json( Response ),
				Status_Code	=> Status_Code,
				Cache_Control	=> Cache_Control,
				Wrap_Data	=> Wrap_Data
				);
	end Build_Redirect_Response;



	function Build(
			Message_Body	: in String;
			Status_Code	: in AWS.Messages.Status_Code;
			Cache_Control	: in AWS.Messages.Cache_Option;
			Wrap_Data	: in Boolean
		) return AWS.Response.Data is
		-- do the final building process (where wrap_data is actually used)
	begin
		if Wrap_Data then
			return AWS.Response.Build(
						Content_Type	=> "text/html",
						Message_Body	=> "<html><body><textarea>" & Message_Body & "</textarea></body></html>",
						Status_Code	=> Status_Code,
						Cache_Control	=> Cache_Control
					);
		else
			return AWS.Response.Build(
						COntent_Type	=> "application/json",
						Message_Body	=> Message_Body,
						Status_Code	=> Status_Code,
						Cache_Control	=> Cache_Control
					);
		end if;
	end Build;


end KOW_View.json_util;
