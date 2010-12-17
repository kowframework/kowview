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

------------------------------------------------------------------------------
-- Main package for KOW_View                                                --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Exceptions;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;
with KOW_Sec.Accounting;
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Components.Registry;
with KOW_View.Json_Util;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Status;
with AWS.Response;


package body KOW_View is


	function Process_Request( Request : in AWS.Status.Data ) return AWS.Response.Data is
		-- this is the main function... it's the AWS callback used all around.
		-- notice that in the v2.0 release the package KOW_View.Service_Mappings was extinguished



		Response	: AWS.Response.Data;
		Component	: Component_Access := Registry.Get_Component( Request );

		function Request_Mode return Request_Mode_Type is
			Params	: AWS.Parameters.List := AWS.Status.Parameters( Request );
		begin
			return Request_Mode_Type'Value( AWS.Parameters.Get( Params, "mode" ) );
		exception
			when others => return Custom_Request;
		end Request_Mode;
	begin
		case Request_Mode is
			when Json_Request =>
				declare
					Object : KOW_Lib.Json.Object_Type;
				begin
					Process_JSon_Request(
							Component	=> Component.all,
							Request		=> Request,
							Response	=> Object 
						);

					Response := KOW_View.Json_Util.Build_Success_Response( Object );
				exception
					when e : others =>
						Response := KOW_View.Json_Util.Build_Error_Response( E );
				end;

			when Custom_Request =>
				Process_Custom_Request(
						Component	=> Component.all,
						Request		=> Request,
						Response	=> Response
					);
		end case;

		return Response;
	exception
		when REDIRECT_TO_HOME =>
			return AWS.Response.URL( To_String( Home ) );
		when E : others =>
			Ada.Exceptions.Reraise_Occurrence( E );
			-- TODO :: implement a nice exception page with some cool stuff, such as showing up a friendly error message to the user :)
	end Process_Request;


end KOW_View;
