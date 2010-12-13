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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------


---------
-- Ada --
---------
with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------

with KOW_Config;
with KOW_View.Components.Registry;
with KOW_View.Pages;

---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;
with AWS.Session;
with AWS.Status;
with Templates_Parser;



package body KOW_View.Navigation is

	----------------
	-- Components --
	----------------
	


	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		) is
		-- Initializie the component while starting up the server
		-- Config is an already initialized configuration file located at:
		-- 	kowview/component_name
		--
		-- Component Options:
		-- 	default_menu_template	=> the template name for the link module
	begin
		Component.Default_menu_template := KOW_Config.Value(
						F	=> Config,
						Key	=> "default_menu_template",
						Default	=> Component.Default_menu_template
					);
	end Initialize;



	function Create_Menu_Module_Instance(
			Component	: in Component_Type;
			Config		: in KOW_Config.Config_File
		) return Menu_Module_Type is

		Configs : KOW_Config.Config_File_Array := KOW_Config.Elements_Array( Config, "link" );
		Link	: Link_Descriptor_Type;

		Module	: Menu_Module_Type;
	begin

		Module.Template := KOW_Config.Value(
						F	=> Config,
						Key	=> "template",
						Default	=> Component.Default_menu_template
					);

		for i in Configs'First .. Configs'Last loop
			Link.Label := KOW_Config.Element( Configs( i ), "label" );
			Link.Href  := KOW_Config.Element( Configs( i ), "href"  );
			Link.Level := Positive( KOW_Config.Value( Configs( i ), "level", 1 ) );

			Link_Vectors.Append( Module.Links, Link );
		end loop;

		return Module;
	exception
		when CONSTRAINT_ERROR => raise KOW_View.Components.MODULE_ERROR with "big bad issue while building a menu... something is missing in here";
	end Create_Menu_Module_Instance;




	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return Module_Type'Class is
		-- no matter what module we request, the Menu_Module_Type_Module will be always called
	begin
		return Create_Menu_Module_Instance( Component, Config );
	end Create_Instance;


	type DUMB_SERVICE is new KOW_View.Components.Service_Type with null record;

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Type'Class is
		D:DUMB_SERVICE;
	begin
		raise KOW_View.Components.SERVICE_ERROR with "no service " & Service_Name & " available";

		return D;
	end Create_Instance;




	overriding
	procedure Process_Request(
			Module		: in out Menu_Module_Type;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is 
		-- print the menu
		
		use Templates_Parser;

		Template_Path : String := KOW_View.Components.Registry.Locate_Resource(
							Component_Name  => "navigation",
							Resource        => To_String( Module.Template ),
							Extension       => "html",
							Kind            => Ada.Directories.Ordinary_File
						);
	
		Labels_Tag	: Templates_Parser.Tag;
		Hrefs_Tag	: Templates_Parser.Tag;
		Levels_Tag	: Templates_Parser.Tag;
		Has_Access_Tag	: Templates_Parser.Tag;
		My_Parameters	: Templates_Parser.Translate_Set := Parameters;

		Page_Mapping	: String := "/pages";


		function Has_Page_Access( Page : in String ) return Boolean is
			use KOW_View.Components.Registry;

	                Module : Module_Type'Class :=
					KOW_View.Components.Registry.Load_Module(
							Component_Name	=> "pages",
							Module_Name	=> "page",
							Config		=> KOW_View.Pages.Load_Page_Config( Page )
						);
						
			Text_Output     : Unbounded_String;
			Is_Final        : Boolean;
			Response_Data	: AWS.Response.Data;
		begin
			KOW_View.Pages.Initialize_Request(
				Module          	=> KOW_View.Pages.Page_Module'Class( Module ),
				Request         	=> Request,
				Parameters      	=> Parameters,
				Response        	=> Response_Data,
				Is_Final        	=> Is_Final,
				Initialize_Modules_Only	=> True
				);
			return not Is_Final;
		end Has_Page_Access;

		function Has_Access( Href : in String ) return Boolean is
			First	: Integer := Href'First;
			Last	: Integer := Href'First + Page_Mapping'Length - 1;
		begin
			if Href( First .. Last ) /= Page_Mapping then
				return True;
			else
				return Has_Page_Access( Href( Last + 1 .. Href'Last ) );
			end if;
		exception
			when Constraint_Error => return True;
		end Has_Access;


		procedure Iterator( C : Link_Vectors.Cursor ) is
			Link : Link_Descriptor_Type := Link_Vectors.Element( C );
		begin
			Labels_Tag := Labels_Tag & To_String( Link.Label );
			Hrefs_Tag := Hrefs_Tag & To_String( Link.Href );
			Levels_Tag := Levels_Tag & Link.Level;
			Has_Access_Tag := Has_Access_Tag & Has_Access( To_String( Link.Href ) );
		end Iterator;
	begin
		
		Link_Vectors.Iterate( Module.Links, Iterator'Access );
	
		Templates_Parser.Insert( My_Parameters, Assoc( "menu_item_label", Labels_Tag ) );
		Templates_Parser.Insert( My_Parameters, Assoc( "menu_item_href", Hrefs_Tag ) );
		Templates_Parser.Insert( My_Parameters, Assoc( "menu_item_level", Levels_Tag ) );
		Templates_Parser.Insert( My_Parameters, Assoc( "menu_has_access", Has_Access_Tag ) );

		Response := Response & To_Unbounded_String(
						Templates_Parser.Parse( Template_Path, My_Parameters )
					);
	end Process_Request;




end KOW_View.Navigation;
