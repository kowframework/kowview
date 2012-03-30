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
-- Main package for KOW Text Markup Language processing                     --
------------------------------------------------------------------------------



-------------
-- XML/Ada --
-------------
with DOM.Core;


-- The KTML is a superset of the XHTML language. Actually, it's just a
-- smart way to deal with XML files.

package body KOW_View.DOM_Util is


	procedure Clone_Child_Nodes(
				Old_Parent	: in Node;
				New_Parent	: in Node
			) is
		-- clone all the child nodes from old_parent to new_parent;

		Children : Node_List := Nodes.Child_Nodes( Old_Parent );
	begin
		for i in 0 .. Length( Children ) - 1 loop
			Nodes.Append_Child( New_Parent, Nodes.Clone( Item( Children, i ) ) );
		end loop;
	end Clone_Child_Nodes;


	procedure Clone_Attributes(
				Old_Parent	: in Node;
				New_Parent	: in Node
			) is
		use Elements;
		-- clone all the element attributes from old_parent to new_parent

		Old_Attributes	: Named_Node_Map := Nodes.Attributes( Old_Parent );
		Attribute	: Node;
	begin
		for i in 0 .. Length( Old_Attributes ) - 1 loop
			Attribute := Item( Old_Attributes, i );
			Set_Attribute( New_Parent, Nodes.Node_Name( Attribute ), Nodes.Node_Value( Attribute ) );
		end loop;
	end Clone_Attributes;


	function Node_Attribute(
				N	: in Node;
				Key	: in String;
				Default	: in String 
			) return String is
		-- get the node attribute if existing; if not, return the default value;
		Value : constant String := Elements.Get_Attribute( N, Key );
	begin
		if Value = "" then
			return Default;
		else
			return Value;
		end if;
	end Node_Attribute;

end KOW_View.DOM_Util;
