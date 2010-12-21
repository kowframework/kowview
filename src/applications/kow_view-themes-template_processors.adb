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
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;



-------------------
-- KOW Framework --
-------------------
with KOW_Lib.UString_Vectors;


---------
-- AWS --
---------
with Templates_Parser;			use Templates_Parser;

package body KOW_View.Themes.Template_Processors is



	-----------------------
	-- The Region Buffer --
	-----------------------


	function ID(
			Region_Buffer	: in Region_Buffer_Type;
			Block		: in Block_Type;
			Module_ID	: in Positive
		) return String is

		The_ID_Str : constant String := Ada.Strings.Fixed.Trim( Positive'Image( Module_ID ), Ada.Strings.Both );
	begin
		return Block_Type'Image( Block ) & '_' & To_String( Region_Buffer.Region ) & "_module_" & The_ID_Str ;
	end ID;


	procedure Append_Head(
				Region_Buffer	: in out Region_Buffer_Type;
				Module_ID	: in     Positive;
				Head_Buffer	: in     Unbounded_String
			) is
	begin
		Region_Buffer.Head_IDs		:= Region_Buffer.Head_IDs & ID( Region_Buffer, Head_Block, Module_ID );
		Region_Buffer.Head_Buffer	:= Region_Buffer.Head_Buffer & Head_Buffer;
	end Append_Head;
		
	
	procedure Append_Body(
				Region_Buffer	: in out Region_Buffer_Type;
				Module_ID	: in     Positive;
				Body_Buffer	: in     Unbounded_String
			) is
	begin
		Region_Buffer.Body_IDs		:= Region_Buffer.Body_IDs & ID( Region_Buffer, Body_Block, Module_ID );
		Region_Buffer.Body_Buffer	:= Region_Buffer.Body_IDs & Body_Buffer;
	end Append_Body;
	

	procedure Append_Foot(
				Region_Buffer	: in out Region_Buffer_Type;
				Module_ID	: in     Positive;
				Foot_Buffer	: in     Unbounded_String
			) is
	begin
		Region_Buffer.Foot_IDs		:= Region_Buffer.Foot_IDs & Id( Region_Buffer, Foot_Block, Module_ID );
		Region_Buffer.Foot_Buffer	:= Region_Buffer.Foot_Buffer & Foot_Buffer;
	end Append_Foot;


	-----------------------------
	-- Region Buffer and Index --
	-----------------------------


	function New_Template_Processor( Template : in KOW_View.Themes.Template_Type ) return Template_Processor_Type is
		-- return an initialized template processor object.. ready to use :)
		
		Processor	: Template_Processor_Type;
		Current_Region	: Positive := 1;
		procedure Initialize_Regions( C : in KOW_Lib.UString_Vectors.Cursor ) is
			Region : constant Region_Type := Region_Type( KOW_Lib.UString_Vectors.Element( C ) );
		begin
			Processor.Buffers( Current_Region ).Region := Region;
			Region_Index_Maps.Include(
						Processor.Index_Map,
						Region,
						Current_Region
					);
			Current_Region := Current_Region + 1;
		end Initialize_Regions;
	begin
		Processor.Template := Template;
		KOW_Lib.UString_Vectors.Iterate( Processor.Template.Regions, Initialize_Regions'Access );

		return Processor;
	end New_Template_Processor;



	function Region_ID(
				Processor	: in Template_Processor_Type;
				Region		: in Region_Type
			) return Positive is
		-- get the ID or raise constraint error with informative message
	begin
		return Region_Index_Maps.Element( Processor.Index_Map, Region );
	exception
		when CONSTRAINT_ERROR =>
			raise CONSTRAINT_ERROR with "there is no region " & To_String( Region ) & " in this template";
	end Region_ID;


	procedure Append_Head(
				Processor	: in out Template_Processor_Type;
				Region		: in     Region_Type;
				Module_ID	: in     Positive;
				Head_Buffer	: in     Unbounded_String
			) is
	begin
		Append_Head(
				Region_Buffer	=> Processor.Buffers( Region_ID( Processor, Region ) ),
				Module_ID	=> Module_ID,
				Head_Buffer	=> Head_Buffer
			);
	end Append_Head;

	procedure Append_Body(
				Processor	: in out Template_Processor_Type;
				Region		: in     Region_Type;
				Module_ID	: in     Positive;
				Body_Buffer	: in     Unbounded_String
			) is
	begin
		Append_Body(
				Region_Buffer	=> Processor.Buffers( Region_ID( Processor, Region ) ),
				Module_ID	=> Module_ID,
				Body_Buffer	=> Body_Buffer
			);
	end Append_Body;



	procedure Append_Foot(
				Processor	: in out Template_Processor_Type;
				Region		: in     Region_Type;
				Module_ID	: in     Positive;
				Foot_Buffer	: in     Unbounded_String
			) is
	begin
		Append_Foot(
				Region_Buffer	=> Processor.Buffers( Region_ID( Processor, Region ) ),
				Module_ID	=> Module_ID,
				Foot_Buffer	=> Foot_Buffer
			);
	end Append_Foot;

	

	procedure Process(
				Processor	: in out Template_Processor_Type;
				Output		:    out Unbounded_String
			) is
	begin
		-- TODO :: the process template procedure...
		-- TODO :: I'll implement it later so I can test the logic implementing the page service
		Output := TO_Unbounded_String( "oie" );
	end Process;



end KOW_View.Themes.Template_Processors;
