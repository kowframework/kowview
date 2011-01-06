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
pragma License( GPL );





--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_View.Components;
with KOW_View.Security.Components;



package body KOW_View.Security.REST is



	-------------------------
	-- REST Login Provider --
	-------------------------
	function Get_Link(
				Provider : in REST_Login_Provider_Type
			) return String is
		-- get the link for the provider...
		-- which is the link for the service :D
	begin
		return '/' & KOW_View.Components.Get_Name( Provider.Component.all ) & '/' & To_String( Provider.Service_Name );
	end Get_Link;

	function Get_Icon(
				Provider : in REST_Login_Provider_Type;
				Icon_Size: in Icon_Size_Type
			) return String is
		-- get the icon URL, which should respect:
		-- 	[component_name]/[service_name]_resources/[to_lower(icon_size)].png
	begin
		return '/' & KOW_View.Components.Get_Name( Provider.Component.all ) & '/' &
						To_String( Provider.Service_Name ) & "_resources/" &
						Ada.Characters.Handling.To_Lower( Icon_Size_Type'Image( Icon_Size ) ) & ".png";
	end Get_Icon;



	--------------------------------------
	-- REST Login provider registration --
	--------------------------------------
	
	function Get_Providers return REST_Login_Povider_Vectors.Vector is
		-- return a copy of the providers vector
	begin
		return Providers;
	end Get_Providers;

	procedure Register_Provider(
				Component	: in KOW_View.Components.Component_Access;
				Service_Name	: in String;
				Label		: in String
			) is
		-- register a new provider.... pleace only call it in the main task
		
		Provider : REST_Login_Provider_Type := (
					Component	=> Component,
					Service_name	=> To_Unbounded_String( Service_Name ),
					Label		=> To_Unbounded_String( Label )
				);
	begin
		REST_Login_Povider_Vectors.Append( Providers, provider );
	end Register_Provider;


end KOW_View.Security.REST;
