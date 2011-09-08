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
with KOW_View.Locales;
with KOW_View.Themes.Components;


---------
-- AWS --
---------
with AWS.Messages;
with AWS.Status;
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
		Region_Buffer.Body_Buffer	:= Region_Buffer.Body_Buffer & Body_Buffer;
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


	procedure Insert(
				Parameters	: in out Templates_Parser.Translate_Set;
				Region_Buffer	: in     Region_Buffer_Type
			) is
		use Templates_Parser;

		Preffix : constant String := To_String( Region_Buffer.Region ) & '_';


		procedure I( Name : in String; What : in Templates_Parser.Tag ) is
		begin
			Insert( Parameters, Assoc( Preffix & Name, What ) );
		end I;

	begin
		I( "head_ids",	Region_Buffer.Head_IDs );
		I( "heads",	Region_Buffer.Head_Buffer );

		I( "body_ids",	Region_Buffer.Body_IDs );
		I( "bodies",	Region_Buffer.Body_Buffer );

		I( "foot_ids",	Region_Buffer.Foot_IDs );
		I( "feet",	Region_Buffer.Foot_Buffer );
	end Insert;
		



	---------------------
	-- Include Buffers --
	---------------------
	
	procedure Insert(
			Parameters	: in out Translate_Set;
			Include_Buffers	: in     Include_Buffers_Type
		) is
	
		procedure Append_All(
				From	: in     KOW_Lib.UString_Vectors.Vector;
				To	: in out Templates_Parser.Tag
			) is
			use Templates_Parser;
			procedure Iterator( C : in KOW_Lib.UString_Vectors.Cursor ) is
			begin
				To := To & KOW_Lib.UString_Vectors.Element( C );
			end Iterator;
		begin
			KOW_Lib.UString_Vectors.Iterate( From, Iterator'Access );
		end Append_All;

		procedure Insert_All(
					Name	: in String;
					From	: in KOW_Lib.UString_Vectors.Vector
				) is
			The_Tag : Templates_Parser.Tag;
		begin
			Append_all( From => From, To => The_Tag );
			Insert( Parameters, Assoc( Name, The_Tag ) );
		end Insert_All;
	begin
		Insert_All( "script_includes", Include_Buffers.Script_Includes );
		Insert_All( "dojo_packages", Include_Buffers.Dojo_Packages );
		Insert_All( "dojo_css", Include_Buffers.Dojo_CSS );
		Insert_All( "css_includes", Include_Buffers.CSS_Includes );
	end Insert;

	
	procedure Append_Unique(
				From	: in     KOW_Lib.UString_Vectors.Vector;
				To	: in out KOW_Lib.UString_Vectors.Vector
			) is
		use KOW_Lib.UString_Vectors;
		procedure Append_Iterator( C : in Cursor ) is
			E : Unbounded_String := Element( C );
		begin
			if not Contains( To, E ) then
				Append( To, E );
			end if;
		end Append_Iterator;
	begin
		Iterate( From, Append_Iterator'Access );
	end Append_Unique;
	
	------------------------
	-- Template Processor --
	------------------------





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


	procedure Append_Script_Includes(
				Processor	: in out Template_Processor_Type;
				Script_Includes	: in     KOW_Lib.UString_Vectors.Vector
			) is
	begin
		Append_Unique(
				From	=> Script_Includes,
				To	=> Processor.Include_Buffers.Script_Includes
			);
	end Append_Script_Includes;

	procedure Append_Dojo_Packages(
				Processor	: in out Template_Processor_Type;
				Dojo_Packages	: in     KOW_Lib.UString_Vectors.Vector
			) is
	begin
		Append_Unique(
				From	=> Dojo_Packages,
				To	=> Processor.Include_Buffers.Dojo_Packages
			);
	end Append_Dojo_Packages;

	procedure Append_Dojo_CSS(
				Processor	: in out Template_Processor_Type;
				Dojo_CSS	: in     KOW_Lib.Ustring_Vectors.Vector
			) is
	begin
		Append_Unique(
				From	=> Dojo_CSS,
				To	=> Processor.Include_Buffers.Dojo_CSS
			);
	end Append_Dojo_CSS;



	procedure Append_CSS_Includes(
				Processor	: in out Template_Processor_Type;
				CSS_Includes	: in     KOW_Lib.UString_Vectors.Vector
			) is
	begin
		Append_Unique(
				From	=> CSS_Includes,
				To	=> Processor.Include_Buffers.CSS_Includes
			);
	end Append_CSS_Includes;





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
				Request		: in     AWS.Status.Data;
				Output		:    out AWS.Response.Data
			) is
		use Templates_Parser;
		Parameters	: Templates_Parser.Translate_Set;
		Template_File	: constant String := Get_File_Name( Processor.Template, Processor.Virtual_Host, Request );

		procedure Insert_Regions_Iterator( C : Region_Index_Maps.Cursor ) is
		begin
			Insert( Parameters, Processor.Buffers( Region_Index_Maps.Element( C ) ) );
		end Insert_Regions_Iterator;
	begin
		Insert( Parameters, Assoc( "title", Processor.Title ) );
		Insert( Parameters, Assoc( "author", Processor.Author ) );
		Insert( Parameters, Assoc( "dojo_locale", KOW_View.Locales.Get_Dojo_Locale( Request ) ) );
		Insert( Parameters, Processor.Include_Buffers );

		Region_Index_Maps.Iterate( Processor.Index_Map, Insert_Regions_Iterator'Access );


		declare
			contents : constant String := Templates_Parser.Parse( Template_File, Parameters );
		begin
			Output := AWS.Response.Build( 
						Content_Type	=> "text/html", 
						Message_Body	=> Contents
--						Encoding	=> AWS.Messages.Deflate
					);
		end;
	end Process;



end KOW_View.Themes.Template_Processors;
