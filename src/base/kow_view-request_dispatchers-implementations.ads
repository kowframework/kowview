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


package KOW_Sec.Request_Dispatchers.Implementations is

	--------------------------
	-- Base Dispatcher Type --
	--------------------------

	type Base_Dispatcher_Type is abstract new Request_Dispatcher_Interface with private;

	procedure Setup_Status(
				Dispatcher	: in     Base_Dispatcher_Type;
				Request		: in     AWS.Status.Data;
				Status		: in out Request_Status_Type
			);
	-- initialize the following request status attributes:
	-- 	* mode
	-- 	* request

	----------------------------
	-- Prefix Dispatcher Type --
	----------------------------

	type Prefix_Dispatcher_Type is abstract new Request_Dispatcher_Interface with private;


	overriding
	function Can_Dispatch(
				Dispatcher	: in Prefix_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return Boolean;
	


	overriding
	procedure Setup_Status(
				Dispatcher	: in     Prefix_Dispatcher_Type;
				Request		: in     AWS.Status.Data;
				Status		: in out Request_Status_Type
			);
	-- initialize the following request status attributes:
	-- 	* mode
	-- 	* request
	-- 	* mapped_uri
	-- 	* mapped_expression
	-- 	* local_uri

	procedure Set_Prefix(
				Dispatcher	: in out Prefix_Dispatcher_Type;
				Prefix		: in     String
			);


private

	type Prefix_Dispatcher_Type is abstract new Request_Dispatcher_Interface with record
		-- the request dispatcher is a singleton object


		Prefix		: String_Access;
		Prefix_Length	: Natural := 0;
	end record;


end KOW_Sec.Request_Dispatchers.Implementations;
