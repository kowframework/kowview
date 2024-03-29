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



--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Tags;

-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Lib.File_System;		use KOW_Lib.File_System;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Json_Util;
with KOW_View.Util;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;

package body KOW_View.Services is


	------------------
	-- Service_Type --
	------------------


	function Locate_Resource(
			Service		: in Service_Type;
			Status		: in Request_Status_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
		Prefix : constant String := To_String( Get_Name( Service ) ) & "_service";
	begin
		return Locate_Resource(
					Component	=> Service.Component.all,
					Status		=> Status,
					Resource	=> Prefix / Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);
	end Locate_Resource;



	function Load_Config(
			Service	: in Service_Type;
			N	: in String := "setup"
		) return KOW_Config.Config_File_Type is
		use KOW_Lib.File_System;
	begin
		return KOW_View.Components.Load_Config(
							Component	=> Service.Component.all,
							N		=> To_String( Get_Name( Service_Type'Class( Service ) ) ) & "_service" / N
						);
	end Load_Config;




	function Get_Name( Service : in Service_Type ) return Service_Name is
	begin
		return KOW_View.Util.Get_Type_Name( Service_Type'Class( Service )'Tag, "_service" );
	end Get_Name;



	-------------------------
	-- Service Dispatchers --
	-------------------------

	overriding
	function Dispatch(
				Dispatcher	: in Service_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return AWS.Response.Data is
		Status		: Request_Status_Type;
		Response	: AWS.Response.Data;
	begin
		Setup_Status( Dispatcher, Request, Status );
		declare
			Service		: Service_Ptr;
		begin
			Create( Dispatcher.Factory.all, Status, Service );
			case Status.Mode is
				when Custom_Request =>
					Process_Custom_Request(
								Service		=> Service.all,
								Status		=> Status,
								Response	=> Response
							);
				when Json_Request =>
					declare
						JResponse : KOW_Lib.Json.Object_Type;
						Wrap_Data : constant Boolean := AWS.Parameters.Get( AWS.Status.Parameters( Status.Request ), "iframe" ) = "true";
					begin
						Process_Json_Request(
									Service		=> Service.all,
									Status		=> Status,
									Response	=> JResponse
								);
						Response := KOW_View.Json_Util.Build_Success_Response( Object => JResponse, Wrap_Data => Wrap_Data );
					end;
			end case;
			Destroy( Dispatcher.Factory.all, Status, Service );
			return Response;
		exception
			when e : others =>
				if Service /= null then
					Destroy( Dispatcher.Factory.all, Status, Service );
				end if;
				Ada.Exceptions.Reraise_Occurrence( e );
		end;
	end Dispatch;


end KOW_View.Services;
