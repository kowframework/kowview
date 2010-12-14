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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Delegator implementation for Stateful services                          --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Finalization;

------------------
-- KOW Famework --
------------------
with KOW_Lib.Json;
with KOW_View.Components;

---------
-- AWS --
---------
with AWS.Response;
with AWS.Session;
with AWS.Status;


generic
	type Service_Type is new KOW_View.Components.Service_Type with private;
	Component	: KOW_View.Components.Component_Access;
package KOW_View.Services.Stateful_Service_Cycles is
pragma Elaborate_Body( KOW_View.Services.Stateful_Service_Cycles );




	---------------------------
	-- The Service Container --
	---------------------------
	

	type Service_Container_Type is record
		Service	: Service_Type := ( Component => Component, others => <> );
		Is_Null	: Boolean := True;
	end record;

	Null_Service_Container	: constant Service_Container_Type := (
							Is_null => true,
							Service => (
									Component	=> Component,
									others		=> <>
								)
						);

	Service_Container_Key	: constant String := 
		KOW_View.Components.Get_Name( Component ) & "::" & KOW_View.Util.Get_Type_name( Service_Type'Tag ) & "::state";
	-- the key inside the session


	package Service_Container_Data is new AWS.Session.Generic_Data(
			Data		=> Service_Container_Type,
			Null_Data	=> Null_Service_Container
		);

	
	function Get( Request : in AWS.Status.Data ) return Service_Container_Type;
	procedure Set( Request : in AWS.Status.Data; Container : in Service_Container_Type );


	-------------------
	-- The Delegator --
	-------------------

	type Service_Delegator_Type is new KOW_View.Components.Service_Delegator_Interface with null record;


	overriding
	procedure Process_Json_Request(
			Delegator	: in out Service_Delegator_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		);

	overriding
	procedure Process_Custom_Request(
			Delegator	: in out Service_Delegator_Type;
			Request		: in     AWS.Status.Data;
			Response	:    out AWS.Response.Data
		);



	---------------
	-- Variables --
	---------------
	
	Delegator : aliased Service_Delegator_Type;


end KOW_View.Services.Stateful_Service_Cycles;
