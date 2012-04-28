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
with DOM.Core;			use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;


-- The KTML is a superset of the XHTML language. Actually, it's just a
-- smart way to deal with XML files.

package body KOW_View.DOM_Util is


	function Deep_Clone( N : in Node ) return Node is
		-- a (working) clone that (really) operates within the child nodes
		-- The Clone( N => some_node, Deep => True ) function in XML/Ada don't
		-- actually clone the child nodes but pass references (at least for text nodes).
		--
		-- this one is slower, but works as expected;
		New_N : Node;
	begin
		case N.Node_Type is
			when Element_Node =>
				New_N := Documents.Create_Element( Nodes.Owner_Document( N ), Elements.Get_Tag_Name( N ) );
				Clone_Child_Nodes( Old_Parent => N, New_Parent => New_N );
				Clone_Attributes(  Old_Parent => N, New_Parent => New_N );

			when Text_Node =>
				declare
					Str : DOM_String := Nodes.Node_Value( N );
				begin
					New_N := Documents.Create_Text_Node( Nodes.Owner_Document( N ), Str );
				end;
			when others =>
				New_N := Nodes.Clone_Node( N => N, Deep => False );
		end case;

		return New_N;
	end Deep_Clone;


	procedure Clone_Child_Nodes(
				Old_Parent	: in Node;
				New_Parent	: in Node
			) is
		-- clone all the child nodes from old_parent to new_parent;

		Children : Node_List := Nodes.Child_Nodes( Old_Parent );
		Child    : Node;
	begin
		for i in 0 .. Nodes.Length( Children ) - 1 loop
			Child := Nodes.Append_Child( New_Parent, Deep_Clone( Nodes.Item( Children, i ) ) );
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
		for i in 0 .. Nodes.Length( Old_Attributes ) - 1 loop
			Attribute := Nodes.Item( Old_Attributes, i );
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



	function Create_From_Template(
				Doc		: in Document;
				Template_Node	: in Node;
				Default_Tag	: in String;
				Deep		: in Boolean
			) return Node is
		-- creates a new element node from the template_node
		-- the new node will be of the tag attribute of the given node or default_Tag
		-- if deep, clone child node as well

		Tag_Att : constant String := "tag";
		Keep_Att: constant String := "keep";
		N       : Node;
		Keep	: constant Boolean := Boolean'Value( Node_Attribute( Template_Node, Keep_Att, "true" ) );

	begin


		if Keep then
			N := Documents.Create_Element( Doc, Node_Attribute( Template_node, Tag_Att, Default_Tag ) );
			Clone_Attributes( Old_Parent => Template_Node, New_Parent => N );
			Elements.Remove_Attribute( N, Tag_Att );
			Elements.Remove_Attribute( N, Keep_Att );
		else
			N := Documents.Create_Document_Fragment( Doc );
		end if;

		if Deep then
			Clone_Child_Nodes( Old_Parent => Template_Node, New_Parent => N );
		end if;

		return N;
	end Create_From_Template;



	procedure Process_Template(
				Doc		: in     Document;
				N		: in out Node;
				Default_Tag	: in     String;
				Deep		: in     Boolean
			) is
		-- same as Create_From_Template, but instead of simply returning a new node,
		-- replaces the current Node freeing the old one
		New_N : Node := Create_From_Template(
					Doc		=> Doc,
					Template_Node	=> N,
					Default_Tag	=> Default_Tag,
					Deep		=> Deep
				);
	begin
		N := Nodes.Replace_Child(
					N		=> Nodes.Parent_Node( N ),
					Old_Child	=> N,
					New_Child	=> New_N
				);
		Nodes.Free( N, True );
		N := New_N;
	end Process_Template;


end KOW_View.DOM_Util;
