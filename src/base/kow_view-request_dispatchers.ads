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
with AWS.Response;
with AWS.Status;


package KOW_View.Request_Dispatchers is



	----------------------------------
	-- Request Dispatcher Interface --
	----------------------------------

	type Request_Dispatcher_Interface is interface;
	-- the request dispatcher is a singleton object



	type Request_Dispatcher_Ptr is access all Request_Dispatcher_Interface'Class;


	function Can_Dispatch(
				Dispatcher	: in Request_Dispatcher_Interface;
				Request		: in AWS.Status.Data
			) return Boolean is abstract;


	function Dispatch(
				Dispatcher	: in Request_Dispatcher_Interface;
				Request		: in AWS.Status.Data
			) return AWS.Response.Data is abstract;


	----------------------
	-- Dispatcher Queue --
	----------------------


	function Get_Dispatcher( Request : in AWS.Status.Data ) return Request_Dispatcher_Ptr;
	-- try getting the dispatcher; if not found, return null.

	procedure Append_Dispatcher( Dispatcher : in Request_Dispatcher_Ptr );
	-- put the dispatcher into the end of the queue.

private


	type Dispatcher_Node_Type;
	type DIspatcher_Node_Access is access Dispatcher_Node_Type;

	type Dispatcher_Node_Type is record
		Dispatcher	: Request_Dispatcher_Ptr;
		Next		: Dispatcher_Node_Access;
	end record;



	First : Dispatcher_Node_Access;

end KOW_View.Request_Dispatchers;
