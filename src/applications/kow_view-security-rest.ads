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
pragma License( GPL );





------------------------------------------------------------------------------
-- Code for REST login providers such as Facebook and OpenID                --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------
with KOW_View.Components;
with KOW_View.Security.Components;



package KOW_View.Security.REST is



	------------------
	-- Helper Types --
	------------------


	type Icon_Size_Type is(
			Small_Icon,
			Big_Icon
		);
	-- the size of the icon representing this REST provider


	-------------------------
	-- REST Login Provider --
	-------------------------

	type REST_Login_Provider_Type is record
		Component	: KOW_View.Components.Component_Access := KOW_View.Security.Components.Component'Access;
		-- the component with the provider implementation..

		Service_Name	: Unbounded_String;
		-- the service to handle this provider call
		-- it should render or redirect to a HTML page and there is
		-- no need to implement JSON Requests.

		Label		: Unbounded_String;
		-- label for this kind of provider..
	end record;

	function Get_Link(
				Provider : in REST_Login_Provider_Type
			) return String;
	-- get the link for the provider...
	-- which is the link for the service :D


	function Get_Icon(
				Provider : in REST_Login_Provider_Type;
				Icon_Size: in Icon_Size_Type
			) return String;
	-- get the icon URL, which should respect:
	-- 	[component_name]/[service_name]_resources/[to_lower(icon_size)].png



	--------------------------------------
	-- REST Login provider registration --
	--------------------------------------
	
	package REST_Login_Povider_Vectors is new Ada.Containers.Vectors(
						Index_Type	=> Positive,
						Element_Type	=> REST_Login_Provider_Type
					);

	function Get_Providers return REST_Login_Povider_Vectors.Vector;
	-- return a copy of the providers vector

	procedure Register_Provider(
				Component	: in KOW_View.Components.Component_Access;
				Service_Name	: in String;
				Label		: in String
			);
	-- register a new provider.... pleace only call it in the main task


private
	Providers : REST_Login_Povider_Vectors.Vector;
	-- do not access directly because the server is multi-tasking.. containers do not like it

end KOW_View.Security.REST;
