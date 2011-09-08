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
-- Package for objects/modules/stuff that handles the template processing.  --
--                                                                          --
-- The templates are based on the great AWS' template parser.               --
------------------------------------------------------------------------------


--------------
-- Ada 2005 --
--------------
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;

-------------------
-- KOW Framework --
-------------------

---------
-- AWS --
---------
with AWS.Response;
with Templates_Parser;			use Templates_Parser;




package KOW_View.Themes.Template_Processors is


	-----------------------------------------------------------
	-- The blocks inside the template :: head, body and foot --
	-----------------------------------------------------------
	type Block_Type is ( HEAD_BLOCK, BODY_BLOCK, FOOT_BLOCK );

	-----------------------
	-- The Region Buffer --
	-----------------------
	
	type Region_Buffer_Type is record
		Region		: Region_Type;
		
		Head_IDs	: Templates_Parser.Tag;
		Head_Buffer	: Templates_Parser.Tag;

		Body_Ids	: Templates_Parser.Tag;
		Body_Buffer	: Templates_Parser.Tag;

		Foot_Ids	: Templates_Parser.Tag;
		Foot_Buffer	: Templates_Parser.Tag;
	end record;

	procedure Append_Head(
				Region_Buffer	: in out Region_Buffer_Type;
				Module_Id	: in     Positive;
				Head_Buffer	: in     Unbounded_String
			);
	
	procedure Append_Body(
				Region_Buffer	: in out Region_Buffer_Type;
				Module_Id	: in     Positive;
				Body_Buffer	: in     Unbounded_String
			);
	
	procedure Append_Foot(
				Region_Buffer	: in out Region_Buffer_Type;
				Module_Id	: in     Positive;
				Foot_Buffer	: in     Unbounded_String
			);
	
	procedure Insert(
				Parameters	: in out Templates_Parser.Translate_Set;
				Region_Buffer	: in     Region_Buffer_Type
			);


	-----------------------------
	-- Region Buffer and Index --
	-----------------------------

	type Region_Buffer_Array is array( Positive range 1 .. 10 ) of Region_Buffer_Type;
	-- up to 10 regions in each template
	

	package Region_Index_maps is new Ada.Containers.Ordered_Maps(
					Key_Type	=> Region_Type,
					Element_Type	=> Positive
			);

	---------------------
	-- Include Buffers --
	---------------------
	
	type Include_Buffers_Type is record
		Script_Includes	: KOW_Lib.UString_Vectors.Vector;
		Dojo_Packages	: KOW_Lib.UString_Vectors.Vector;
		Dojo_CSS	: KOW_Lib.UString_Vectors.Vector;
		CSS_Includes	: KOW_Lib.UString_Vectors.Vector;
	end record;

	procedure Insert(
			Parameters	: in out Translate_Set;
			Include_Buffers	: in     Include_Buffers_Type
		);
	
	procedure Append_Unique(
				From	: in     KOW_Lib.UString_Vectors.Vector;
				To	: in out KOW_Lib.UString_Vectors.Vector
			);
	
	------------------------
	-- Template Processor --
	------------------------
	type Template_Processor_Type is record
		Index_Map	: Region_Index_Maps.Map;
		Buffers		: Region_Buffer_Array;
		Include_Buffers	: Include_Buffers_Type;

		Template	: KOW_View.Themes.Template_Type;


		Title		: Unbounded_String;
		Author		: Unbounded_String;


		Virtual_Host	: KOW_View.Virtual_Host_Name_Type;
	end record;


	function New_Template_Processor( Template : in KOW_View.Themes.Template_Type ) return Template_Processor_Type;
	-- return an initialized template processor object.. ready to use :)


	procedure Append_Script_Includes(
				Processor	: in out Template_Processor_Type;
				Script_Includes	: in     KOW_Lib.UString_Vectors.Vector
			);

	procedure Append_Dojo_Packages(
				Processor	: in out Template_Processor_Type;
				Dojo_Packages	: in     KOW_Lib.UString_Vectors.Vector
			);

	procedure Append_Dojo_CSS(
				Processor	: in out Template_Processor_Type;
				Dojo_CSS	: in     KOW_Lib.Ustring_Vectors.Vector
			);


	procedure Append_CSS_Includes(
				Processor	: in out Template_Processor_Type;
				CSS_Includes	: in     KOW_Lib.UString_Vectors.Vector
			);



	procedure Append_Head(
				Processor	: in out Template_Processor_Type;
				Region		: in     Region_Type;
				Module_Id	: in     Positive;
				Head_Buffer	: in     Unbounded_String
			);

	procedure Append_Body(
				Processor	: in out Template_Processor_Type;
				Region		: in     Region_Type;
				Module_Id	: in     Positive;
				Body_Buffer	: in     Unbounded_String
			);



	procedure Append_Foot(
				Processor	: in out Template_Processor_Type;
				Region		: in     Region_Type;
				Module_Id	: in     Positive;
				Foot_Buffer	: in     Unbounded_String
			);
	

	procedure Process(
				Processor	: in out Template_Processor_Type;
				Request		: in     AWS.Status.Data;
				Output		:    out AWS.Response.Data
			);



end KOW_View.Themes.Template_Processors;
