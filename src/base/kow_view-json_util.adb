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
with Ada.Exceptions;



-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;			use KOW_Lib.Json;
with KOW_Lib.String_Util;

---------
-- AWS --
---------
with AWS.Messages;
with AWS.Response;

package body KOW_View.json_util is


	function Build_Error_Response(
			E		: Ada.Exceptions.Exception_Occurrence;
			Status_Code	: AWS.Messages.Status_Code := AWS.Messages.S505;
			Cache_Control	: AWS.Messages.Cache_Option := AWS.Messages.No_Cache
		) return AWS.Response.Data is
		
		use Ada.Exceptions;
		use KOW_Lib.String_Util;


		Object	: Object_Type;
	begin
		Set( Object, "status", "error" );
		Set( Object, "error", Exception_Name( e ) );
		Set( Object, "message", Exception_Message( e ) );
		return AWS.Response.Build(
					Content_Type	=> "application/json", 
					Message_Body	=> To_Json( Object ),
					Status_Code	=> Status_Code,
					Cache_Control	=> Cache_Control
			);
	end Build_Error_Response;


	
	function Build_Success_Response(
			Object		: KOW_Lib.Json.Object_Type;
			Status_Code	: AWS.Messages.Status_Code := AWS.Messsages.S200;
			Cache_Control	: AWS.Messages.Cache_Option := AWS.Messages.No_Cache
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
		return AWS.Response.Build(
					Content_Type	=> "application/json", 
					Message_Body	=> To_Json( Response ),
					Status_Code	=> Status_Code
					Cache_Control	=> Cache_Control
			);
	end Build_Sucess_Response;



end KOW_View.json_util;
