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
-- Main package for KOW Text Markup Language processing                     --
------------------------------------------------------------------------------



-------------
-- XML/Ada --
-------------
with DOM.Core;		use DOM.Core;


-- The KTML is a superset of the XHTML language. Actually, it's just a
-- smart way to deal with XML files.

package KOW_View.DOM_Util is



	function Deep_Clone( N : in Node ) return Node;
	-- a (working) clone that (really) operates within the child nodes
	-- The Clone( N => some_node, Deep => True ) function in XML/Ada don't
	-- actually clone the child nodes but pass references (at least for text nodes).
	--
	-- this one is slower, but works as expected;


	procedure Clone_Child_Nodes(
				Old_Parent	: in Node;
				New_Parent	: in Node
			);
	-- clone all the child nodes from old_parent to new_parent;
	

	procedure Clone_Attributes(
				Old_Parent	: in Node;
				New_Parent	: in Node
			);
	-- clone all the element attributes from old_parent to new_parent



	function Node_Attribute(
				N	: in Node;
				Key	: in String;
				Default	: in String 
			) return String;
	-- get the node attribute if existing; if not, return the default value;

	
	function Create_From_Template(
				Doc		: in Document;
				Template_Node	: in Node;
				Default_Tag	: in String;
				Deep		: in Boolean
			) return Node;
	-- creates a new element node from the template_node
	-- the new node will be of the tag attribute of the given node or default_Tag
	-- if deep, clone child node as well
end KOW_View.DOM_Util;
