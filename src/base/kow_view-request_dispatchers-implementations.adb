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

------------------------------------------------------------------------------
-- A request dispatcher is what controlls what should be called             --
-- There are the following dispatchers implemented:                         --
--      * static content dispatcher                                         --
--      * service dispatcher                                                --
--      * page dispatcher                                                   --
------------------------------------------------------------------------------



---------
-- AWS --
---------
with Ada.Response;
with AWS.Status;


package body KOW_Sec.Request_Dispatchers.Implementations is

	--------------------------
	-- Base Dispatcher Type --
	--------------------------


	procedure Setup_Status(
				Dispatcher	: in     Prefix_Dispatcher_Type;
				Request		: in     AWS.Status.Data;
				Status		: in out Request_Status_Type
			) is
		-- initialize the following request status attributes:
		-- 	* mode
		-- 	* request

		Mode_Str : constant String := AWS.Status.Get( Request, "mode" );
	begin
		if Mode /= "" then
			Status.Mode := Request_Mode_Type'Value( Mode );
		else
			Status.Mode := Custom_Request;
		end if;

		Status.Request := Request;
	end Setup_Status;

	----------------------------
	-- Prefix Dispatcher Type --
	----------------------------


	overriding
	function Can_Dispatch(
				Dispatcher	: in Prefix_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return Boolean is
		URL : constant String := AWS.Status.URL( Request );
	begin
		return Prefix /= null and then URL( URL'First .. URL'First + Dispatcher.Prefix_Length - 1 ) = Dispatcher.Prefix.all;
	end Can_Dispatch;


	overriding
	procedure Setup_Status(
				Dispatcher	: in     Prefix_Dispatcher_Type;
				Request		: in     AWS.Status.Data;
				Status		: in out Request_Status_Type
			) is
		-- initialize the following request status attributes:
		-- 	* mode
		-- 	* request
		-- 	* mapped_uri
		-- 	* mapped_expression
		-- 	* local_uri

	
		function Local_URI return String is
			-- return the local URI for the given prefi
			URL : constant String := AWS.Status.URL( Request );
		begin
			return URL( URL'First + Dispatcher.Prefix_Length .. URL'Last );
		end Local_URI;
	begin
		Setup_Status( Base_Dispatcher_Type( Dispatcher ), Request, Status );
		-- mode and request set by superclass

		Copy( From => Dispatcher.Prefix.all, To => Status.Mapped_URI );
		Copy( From => Dispatcher.Prefix.all, To => Status.Mapped_Expression );
		Copy( From => Local_URI, To => Status.Local_URI );
	end Setup_Status;

	procedure Set_Prefix(
				Dispatcher	: in out Prefix_Dispatcher_Type;
				Prefix		: in     String
			) is
	begin
		Dispatcher.Prefix_Length := Prefix'Length;
		Dispatcher.Prefix := new String'( Prefix );
	end Set_Prefix;



	-----------------------
	-- htdocs Dispatcher --
	-----------------------

	overriding
	function Can_Dispatch(
				Dispatcher	: in Htdocs_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return Boolean is
		-- check if the given URI exists inside the URI folder
		htdocs_path : constant String := Compute_Path( Htdocs_Dispatcher_Type'Class( Dispatcher ), Request );
	begin
		return Ada.Directories.Exists( htdocs_path ) and then Ada.Directories."="( Ada.Directories.Ordinary_File, Ada.Directories.Kind( htdocs_path) ) then
	end Can_Dispatch;


	overriding
	function Dispatch(
				Dispatcher	: in Htdocs_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return AWS.Response.Data is
		-- serve the file using the standard AWS methods
		htdocs_path : constant String := Compute_Path( Htdocs_Dispatcher_Type'Class( Dispatcher ), Request );
	begin
		return AWS.Response.File(
					Content_Type    => AWS.MIME.Content_Type( htdocs_path ),
					Filename        => htdocs_path
				);
	end Dispatch;


	function Compute_Path(
				Dispatcher	: in Htdocs_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return String is
		use KOW_Lib.File_System;
	begin
		return To_String( Dispatcher.Document_Root ) / AWS.Status.URI( Request );
	end Compute_Path;




	Htdocs_Dispatcher : aliased Htdocs_Dispatcher_Type;
	

begin
	Append_Dispatcher( Htdocs_Dispatcher'Access );

end KOW_Sec.Request_Dispatchers.Implementations;
