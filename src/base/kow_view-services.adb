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
with KOW_View.Services.Util;
with KOW_View.Util;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;
with Templates_Parser;

package body KOW_View.Services is
	----------------------------
	-- Service Delegator Type --
	----------------------------

	type Service_Dispatcher_Type is new KOW_View.Request_Dispatchers.Implementations.Prefix_Request_Dispatcher_Type with record
		Component_Name	: Component_Name_Type;
		Component	: Components.Component_Ptr;
		Service_Name	: Service_Name_Type;
	end record;



	overriding
	function Dispatch(
				Dispatcher	: in Service_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return AWS.Response.Data is
		Status		: Request_Status_Type;
	begin
		Setup_Status( Dispatcher, Status );
		declare
			use KOW_Config.Components;
			Delegator	: Service_Delegator_Access := Get_Service_Delegator( Dispatcher.Component.all, Dispatcher.Service_Name );
		begin

			case Status.Mode is
				when Custom_Request =>
					declare
						Response : AWS.Response.Data;
					begin
						Process_Custom_Request(
									Delegator	=> Delegator.all,
									Status		=> Status,
									Response	=> Response
								);
						return Response;
					end;
				when Json_Request =>
					declare
						Response : KOW_Lib.Json.Object_Type;
						Wrap_Data : constant Boolean := AWS.Parameters.Get( AWS.Status.Parameters( Status.Request ), "iframe" ) = "true";
					begin
						Process_Json_Request(
									Delegator	=> Delegator.all,
									Status		=> Status,
									Response	=> Response
								);
						return KOW_View.Json_Util.Build_Success_Response( Object => Response, Wrap_Data => Wrap_Data );
					end;
			end case;
		end;
	end Dispatch;

	


	overriding
	procedure Setup_Status(
				Dispatcher	: in     Base_Dispatcher_Type;
				Request		: in     AWS.Status.Data;
				Status		: in out Request_Status_Type
			) is
		use KOW_View.Request_Dispatchers.Implementations;
	begin
		Setup_Status(
				Dispatcher	=> Path_Request_Dispatcher_Type( Dispatcher ),
				Status		=> Status
			);
		Status.Component := Dispatcher.Component_Name;
		Status.Service   := Dispatcher.Service_Name;
	end Setup_Status;



	-------------
	-- Service --
	-------------

	function Locate_Resource(
			Service		: in Service_Type;
			Resource	: in String;
			Extension	: in String := "";
			Virtual_Host	: in String;
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File;
			Locale		: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return String is
		Prefix : constant String := Get_Name( Service ) & "_service";
	begin
		return Locate_Resource(
					Component	=> Service.Component.all,
					Resource	=> Prefix / Resource,
					Extension	=> Extension,
					Virtual_Host	=> Virtual_host,
					Kind		=> Kind,
					Locale		=> Locale
				);
	end Locate_Resource;


	procedure Setup_Service(
			Component	: in     Component_Access;
			Service		: in out Service_Type'Class
		) is
		-- load the configuration file and run setup..
	begin
		Service.Component := Component_Ptr( Component );
		declare
			use KOW_Config;
			use KOW_view.Util;
			Config : Config_File_Type := New_Config_File(
							Get_Name( Component.all ) / Get_Name( Service )
						);
		begin
			Setup_Service( Service, Config );
		end;
	exception
		when KOW_Config.FILE_NOT_FOUND => null;
	end Setup_Service;



	function Parse_Template(
			Service			: in Service_Type;
			Template_Resource	: in String;
			Template_Extension	: in String := "";
			Virtual_Host		: in String;
			Parameters		: in Templates_Parser.Translate_Set;
			Locale			: in KOW_Lib.Locales.Locale_Type := KOW_Lib.Locales.Get_Default_Locale
		) return String is
		-- helper method for calling templates parser's parse method and locate_resource

		
		Resource : constant String := Locate_Resource(
						Service		=> Service_Type'Class( Service ),
						Resource	=> Template_Resource,
						Extension	=> Template_Extension,
						Virtual_Host	=> Virtual_Host,
						Kind		=> Ada.Directories.Ordinary_File,
						Locale		=> Locale
					);
	begin
		return Templates_Parser.Parse( Resource, Parameters );
	end Parse_Template;


	function Local_URI(
				Service	: in Service_Type;
				URI	: in String;
				No_Slash: in Boolean := False
			) return String is
	begin
		return Util.Local_URI(
					Service	=> Service_Type'Class( Service ),
					URI	=> URI,
					No_Slash=> No_Slash
				);
	end Local_URI;


	function Get_Name( Service : in Service_Type'Class ) return String is
	begin
		return KOW_View.Services.Util.Get_Name( Service'Tag );
	end Get_Name;


end KOW_View.Services;
