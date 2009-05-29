

---------
-- Ada --
---------
with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_View.Components_Registry;

---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;
with AWS.Session;
with AWS.Status;
with Templates_Parser;



package body Aw_View.Navigation is

	----------------
	-- Components --
	----------------
	


	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		) is
		-- Initializie the component while starting up the server
		-- Config is an already initialized configuration file located at:
		-- 	awview/component_name
		--
		-- Component Options:
		-- 	default_menu_template	=> the template name for the link module
	begin
		Component.Default_menu_template := Aw_Config.Value(
						F	=> Config,
						Key	=> "default_menu_template",
						Default	=> Component.Default_menu_template
					);
	end Initialize;



	function Create_Menu_Module_Instance(
			Component	: in Component_Type;
			Config		: in Aw_Config.Config_File
		) return Menu_Module_Type is

		Configs : Aw_Config.Config_File_Array := Aw_Config.Elements_Array( Config, "link" );
		Link	: Link_Descriptor_Type;

		Module	: Menu_Module_Type;
	begin

		Module.Template := Aw_Config.Value(
						F	=> Config,
						Key	=> "template",
						Default	=> Component.Default_menu_template
					);

		for i in Configs'First .. Configs'Last loop
			Link.Label := Aw_Config.Element( Configs( i ), "label" );
			Link.Href  := Aw_Config.Element( Configs( i ), "href"  );

			Link_Vectors.Append( Module.Links, Link );
		end loop;

		return Module;
	exception
		when CONSTRAINT_ERROR => raise Aw_View.Components.MODULE_ERROR with "big bad issue while building a menu... something is missing in here";
	end Create_Menu_Module_Instance;




	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class is
		-- no matter what module we request, the Menu_Module_Type_Module will be always called
		Module: Menu_Module_Type;
	begin
		return Create_Menu_Module_Instance( Component, Config );
	end Create_Instance;


	type DUMB_SERVICE is new Aw_View.Components.Service_Instance_Interface with null record;

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class is
		D:DUMB_SERVICE;
	begin
		raise Aw_View.Components.SERVICE_ERROR with "no service " & Service_Name & " available";

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

		Template_Path : String := Aw_View.Components_Registry.Locate_Resource(
							Component_Name  => "navigation",
							Resource        => To_String( Module.Template ),
							Extension       => "html",
							Kind            => Ada.Directories.Ordinary_File
						);
	
		Labels_Tag	: Templates_Parser.Tag;
		Hrefs_Tag	: Templates_Parser.Tag;
		My_Parameters	: Templates_Parser.Translate_Set := Parameters;

		procedure Iterator( C : Link_Vectors.Cursor ) is
			Link : Link_Descriptor_Type := Link_Vectors.Element( C );
		begin
			Labels_Tag := Labels_Tag & To_String( Link.Label );
			Hrefs_Tag := Hrefs_Tag & To_String( Link.Href );
		end Iterator;
	begin
		
		Link_Vectors.Iterate( Module.Links, Iterator'Access );
	
		Templates_Parser.Insert( My_Parameters, Assoc( "menu_item_label", Labels_Tag ) );
		Templates_Parser.Insert( My_Parameters, Assoc( "menu_item_href", Hrefs_Tag ) );

		Response := Response & To_Unbounded_String(
						Templates_Parser.Parse( Template_Path, My_Parameters )
					);
	end Process_Request;

end Aw_View.Navigation;
