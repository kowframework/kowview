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


package body KOW_Sec.Request_Dispatchers is



	----------------------
	-- Dispatcher Queue --
	----------------------
	function Get_Dispatcher( Request : in AWS.Status.Data ) return Request_Dispatcher_Ptr is
		-- try getting the dispatcher; if not found, return null.
		Element : Dispatcher_Node_Access := First;
	begin

		while Element /= null loop
			if Can_Dispatch( Element.Dispatcher.all, Request ) then
				return Element.Dispatcher;
			else
				Element := Element.Next;
			end if;
		end loop;

		return null;
	end Get_Dispatcher;


	procedure Append_Dispatcher( Dispatcher : in Request_Dispatcher_Ptr ) is
		-- put the dispatcher into the end of the queue.
		function Last( Element : in Dispatcher_Node_Access ) return Dispatcher_Node_Access is
		begin
			if Element = null or else Element.Next = null then
				return Element;
			else
				return Last( Element.Next );
			end if;
		end Last;

		L : Dispatcher_Node_Access := Last( First );
	begin
		if L = null then
			First := new Dispatcher_Node_Type( Dispatcher => Dispatcher, Next => null );
		else
			L.Next := new Dispatcher_Node_Type( Dispatcher => Dispatcher, Next => null );
		end if;
	end Append_Dispatcher;

end KOW_Sec.Request_Dispatchers;
