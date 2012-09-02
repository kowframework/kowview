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


-------------------
-- KOW Framework --
-------------------
with KOW_Sec;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Status;


package KOW_View.Request_Dispatchers.Implementations is

	--------------------------
	-- Base Dispatcher Type --
	--------------------------

	type Base_Dispatcher_Type is abstract new Request_Dispatcher_Interface with record
		-- a base for the request dispatchers you might want to implement
		-- useful
		Criteria : KOW_Sec.Criteria_Ptr;
	end record;

	overriding
	function Login_Required(
				Dispatcher	: in     Base_Dispatcher_Type;
				Request		: in     AWS.Status.Data
			) return Boolean;
	-- return Criteria /= null and then Is_Anonymous
	
	overriding
	function Is_Allowed(
				Dispatcher	: in     Base_Dispatcher_Type;
				Request		: in     AWS.Status.Data
			) return Boolean;
	-- return Criteria = null or else Is_Allowed(Criteria)

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

	type Prefix_Dispatcher_Type is abstract new Base_Dispatcher_Type with private;


	overriding
	function Can_Dispatch(
				Dispatcher	: in Prefix_Dispatcher_Type;
				URI		: in String
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


	---------------------------
	-- Restricted Dispatcher --
	---------------------------


	-----------------------
	-- htdocs Dispatcher --
	-----------------------


	type Htdocs_Dispatcher_Type is new Base_Dispatcher_Type with record
		-- a dispatcher that serves static files from the ./htdocs folder
		Document_Root : Unbounded_String := To_Unbounded_String( "./htdocs" );
	end record;

	overriding
	function Can_Dispatch(
				Dispatcher	: in Htdocs_Dispatcher_Type;
				URI		: in String
			) return Boolean;
	-- check if the given URI exists inside the URI folder


	overriding
	function Dispatch(
				Dispatcher	: in Htdocs_Dispatcher_Type;
				Request		: in AWS.Status.Data
			) return AWS.Response.Data;
	-- serve the file using the standard AWS methods

	function Compute_Path(
				Dispatcher	: in Htdocs_Dispatcher_Type;
				URI		: in String
			) return String;


	Htdocs_Dispatcher : aliased Htdocs_Dispatcher_Type;
	
private

	type Prefix_Dispatcher_Type is abstract new Base_Dispatcher_Type with record
		-- the request dispatcher is a singleton object
		Prefix		: String_Access;
		Prefix_Length	: Natural := 0;
	end record;


end KOW_View.Request_Dispatchers.Implementations;
