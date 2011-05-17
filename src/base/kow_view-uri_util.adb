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


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;

---------
-- AWS --
---------
with AWS.Containers.Tables;
with AWS.Parameters;
with AWS.Status;

package body KOW_View.URI_Util is

	Page_URN_Identifier	: constant String := "page:";
	Page_Service_URI	: constant String := "/pages/page";


	function Is_Page_URN( URN : in String ) return Boolean is
		-- check if it's page:
	begin
		return URN'Length > Page_URN_Identifier'Length and then URN( URN'First .. URN'First + Page_URN_Identifier'Length - 1 ) = Page_URN_Identifier;
	end Is_Page_URN;


	function Get_Page_Name( URN : in String ) return String is
		-- return the page name from the page:// URN (starting with forward slash)
	begin
		if not Is_Page_URN( URN ) then
			raise CONSTRAINT_ERROR with "can't get page name from URN " & URN;
		end if;

		return '/' & URN( URN'First + Page_URN_Identifier'Length .. URN'Last );
	end Get_Page_Name;


	function To_Page_URI( URN : in String ) return String is
	-- convert the given page:// URN into an URI that can be used to access a page
	begin
		return Page_Service_URI & Get_Page_Name( URN );
	end To_Page_URI;


	function Build_URL(
			Request		: in AWS.Status.Data;
			Key1,Value1	: in String;
			Key2,Value2	: in String := "";
			Key3,Value3	: in String := "";
			Key4,Value4	: in String := "";
			Key5,Value5	: in String := "";
			Include_URI	: in Boolean := False
		) return String is
		-- build a URL for the current page replacing the HTTP parameters listed in key/value pairs
		-- maintain all other urls
		use Ada.Strings.Unbounded;
		use AWS.Parameters;


		Buffer		: Unbounded_String;
		Parameters 	: AWS.Parameters.List := AWS.Status.Parameters( Request );
		Has_Param	: Boolean := False;

		procedure Append_Parameter( Key, Value : in String ) is
		begin

			if Value = "" then
				return;
			end if;

			if Has_Param then
				Append( Buffer, '&' );
			else
				Has_Param := True;
			end if;
			Append( Buffer, Key & '=' & Value );
		end Append_Parameter;


		procedure Process( Key, Value : in String ) is
		begin
			if	Key /= Key1 and then
				Key /= Key2 and then
				Key /= Key3 and then
				Key /= Key4 and then
				Key /= Key5 then

				Append_Parameter( Key, Value );
			end if;
		end Process;

		procedure Process_Parameters is new AWS.Containers.Tables.Generic_Iterate_Names( Process => Process );


		function URI return String is
		begin
			if Include_URI then
				return AWS.Status.URI( Request );
			else
				return "";
			end if;
		end URI;
	begin
		Process_Parameters( AWS.Containers.Tables.Table_Type( Parameters ), "," );
		Append_Parameter( Key1, Value1 );
		Append_Parameter( Key2, Value2 );
		Append_Parameter( Key3, Value3 );
		Append_Parameter( Key4, Value4 );
		Append_Parameter( Key5, Value5 );

		return URI & "?" & To_String( Buffer );
	end Build_URL;

end KOW_View.URI_Util;
