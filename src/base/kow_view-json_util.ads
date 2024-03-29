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
-- Some tools for Helping treating JSon Data                                --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Exceptions;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;

---------
-- AWS --
---------
with AWS.Messages;
with AWS.Response;

package KOW_View.json_util is


	-- Notice about Wrap_Data (common in every build functions).
	--
	-- The method Amdjs.io.iframe.send() is the only way to send multipart/form-data to the server.
	-- This method expects the response to be inside an html document such as:
	-- <html>
	--   <body>
	--       <textarea>
	--             payload
	--       </textarea>
	--   </body>
	-- </html>
	--
	-- where payload the is the actual response.
	-- 
	-- Wrap_Data set to true does exactly this.

	function Build_Error_Response(
			E		: Ada.Exceptions.Exception_Occurrence;
			Status_Code	: AWS.Messages.Status_Code := AWS.Messages.S500;
			Cache_Control	: AWS.Messages.Cache_Option := AWS.Messages.No_Cache;
			Wrap_Data	: Boolean := False
		) return AWS.Response.Data;
	-- return a json encoded object with:
	-- 	'status' : 'error'
	-- 	'error'  : Ada.Exceptions.Exception_Name( e )
	-- 	'message': Ada.Exceptions.Exception_Message( e )


	function Build_Success_Response(
			Object		: KOW_Lib.Json.Object_Type;
			Status_Code	: AWS.Messages.Status_Code := AWS.Messages.S200;
			Cache_Control	: AWS.Messages.Cache_Option := AWS.Messages.No_Cache;
			Wrap_Data	: Boolean := False
		) return AWS.Response.Data;
	-- return a json encoded object with
	-- 	'status'  : 'success'
	-- 	'response': To_JSon(object)
	

	function Build_Redirect_Response(
			URI		: String;
			Status_Code	: AWS.Messages.Status_Code := AWS.Messages.S200;
			Cache_Control	: AWS.Messages.Cache_Option := AWS.Messages.No_Cache;
			Wrap_Data	: Boolean := False
		) return AWS.Response.Data;
	-- return a json encoded object with
	-- 	'status'  : 'redirect'
	-- 	'to'      : URI
	-- given that URI can be a page URN such as:
	-- 	page:some/page
	--
	-- or anything else, in wich case isn't touched at all



private
	function Build(
			Message_Body	: in String;
			Status_Code	: in AWS.Messages.Status_Code;
			Cache_Control	: in AWS.Messages.Cache_Option;
			Wrap_Data	: in Boolean
		) return AWS.Response.Data;
	-- do the final building process (where wrap_data is actually used)

end KOW_View.json_util;
