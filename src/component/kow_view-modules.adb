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



--------------
-- Ada 2005 --
--------------
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;





-------------------
-- KOW Framework --
-------------------
with KOW_Lib.File_System;
with KOW_View.Components;	use KOW_View.Components;
with KOW_View.KTML;
with KOW_View.Pages;		use KOW_View.Pages;
with KOW_View.Util;



package body KOW_View.Modules is


	-----------------
	-- Base Module --
	-----------------

	function Get_Name( Module : in Base_Module ) return Module_Name is
	begin
		return KOW_View.Util.Get_Type_Name( Base_Module'Class( Module )'Tag, "_module" );
	end Get_Name;

	function Locate_Resource(
			Module		: in Base_Module;
			Status		: in Request_Status_Type;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
		use KOW_Lib.File_System;
	begin
		return Locate_Resource(
					Component	=> Module.Component.all,
					Status		=> Status,
					Resource	=> To_String( Get_Name( Base_Module'Class( Module ) ) ) & "_module" / Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);
	end Locate_Resource;

	-----------------
	-- KTML Module --
	-----------------

	overriding
	procedure Process_Request(
				Module	: in out KTML_Module;
				Page	: in out Page_Interface'Class;
				Status	: in     Request_Status_Type
			) is
		Initial_State : KOW_Lib.Json.Object_Type;


		function Template return String is
			Tpl : constant String := "template";
			use KOW_Lib.Json;
		begin
			if Contains( Initial_State, Tpl ) then
				return Get( Initial_State, Tpl );
			else
				return "success";
			end if;
		end Template;
	begin
		Process_Json_Request(
					Module	=> KTML_Module'Class( Module ),
					Page	=> Page,
					Status	=> Status,
					Response=> Initial_State
				);
		Append(
				Page	=> Page,
				Content	=> KOW_View.KTML.Render(
								File_Path	=> Locate_Resource(
												Module		=> KTML_Module'Class( Module ),
												Status		=> Status,
												Resource	=> Template,
												Extension	=> "ktml",
												Kind		=> Ada.Directories.Ordinary_File
											),
								Initial_State	=> Initial_State
							)
					);
	end Process_Request;



	-------------------
	-- Static Module --
	-------------------



	package body Static_Modules is

		function Get_Contents ( Path : in String ) return String is
			-- TODO :: figure out how to create a cache of some sort
			use Ada.Strings.Unbounded;
			use Ada.Streams.Stream_IO;

			F : File_Type;
			S : Stream_Access;
			Buffer : Unbounded_String;
		begin

			Open(
					File	=> F,
					Mode	=> In_File,
					Name	=> Path
				);
			S := Stream( F );
			while not End_Of_File( F ) loop
				Append( Buffer, Character'Input( S ) );
			end loop;

			Close( F );
			return To_String( Buffer );
		end Get_Contents;

		overriding
		procedure Process_Request(
					Module	: in out Static_Module;
					Page	: in out Page_Interface'Class;
					Status	: in     Request_Status_Type
				) is

			Path : constant String := Locate_Resource(
									Module		=> Static_Module'Class( Module ),
									Status		=> Status,
									Resource	=> Resource,
									Extension	=> "html",
									Kind		=> Ada.Directories.Ordinary_File
								);
		begin
			Pages.Append(
					Page	=> Page,
					Content	=> Get_Contents( Path )
				);
		end Process_Request;
		
		overriding
		procedure Process_Json_Request(
					Module	: in out Static_Module;
					Page	: in out Page_interface'Class;
					Status	: in     Request_Status_Type;
					Response:    out KOW_Lib.Json.Object_Type
				) is
		begin
			raise program_error with "can't serve static files in JSON requests";
		end Process_Json_Request;


	end Static_Modules;



end KOW_View.Modules;

