	-------------
	-- Modules --
	-------------



	overriding
	procedure Initialize_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		) is
		Session_ID	: constant AWS.Session.ID := AWS.Status.Session (Request);
		Theme_Name	: constant string := AWS.Session.Get(
							Session_ID,
							theme_name_session_key
						);

	begin
	
	
		if Theme_Name /= "" then
			Module.Theme_Name := To_Unbounded_String( Theme_Name );
		else
			Module.Theme_Name := Module.Default_Theme_Name;
		end if;
		
		Module.Render_Start_Timestamp := Ada.Calendar.Clock;

		Is_Final := false;
	end Initialize_Request;
	


	overriding
	procedure Process_Header(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- process header of the response.
		-- it's assumed that 
		use Ada.Calendar;
	begin
		Module.Modules_Finish_Render_Timestamp := Clock;
	end Process_Header;



	procedure Insert_All(
			To	: in out Templates_Parser.Translate_Set;
			Suffix	: in     String;
			Tag_Map	: in out Tag_Maps.Map
		) is
		use Templates_Parser;
	
		procedure Iterator( C: Tag_Maps.Cursor ) is
		begin
			Insert(
				To,
				assoc(
					To_String( Tag_Maps.Key( C ) ) & Suffix,
					Tag_Maps.Element( C )
				)
			);
		end Iterator;
	begin
		Tag_Maps.Iterate( Tag_Map, Iterator'Access );
	end Insert_All;

	overriding
	procedure Process_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- process the request for a module.
		-- sometimes is useful for a module only to be created and released - such as in a page counter module
		use Templates_Parser;
	begin

		Insert( Parameters, assoc( "header", Module.Header_Contents ) );
		Insert( Parameters, assoc( "footer", Module.Footer_Contents ) );

		Insert_All( Parameters, "_header_contents", Module.Module_Header_Contents );
		Insert_All( Parameters, "_contents", Module.Module_Contents );
		Insert_All( Parameters, "_ids", Module.Module_Ids );

		Response := Response & To_Unbounded_String(
					Parse(
						To_String( Module.Template_File_Name ),
						Parameters
					)
				);

	end Process_Request;



	overriding
	procedure Process_Footer(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		use Ada.Calendar;
		Computed_Time : String :=
				Ada.Strings.Fixed.Trim(
					Duration'Image( Clock - Module.Render_Start_Timestamp ),
					Ada.Strings.Both
				);
		Computed_Modules_Time : String :=
				Ada.Strings.Fixed.Trim(
					Duration'Image(
						Module.Modules_Finish_Render_Timestamp - 
						Module.Render_Start_Timestamp
						),
					Ada.Strings.Both
				);
	begin
		Response := Response & To_Unbounded_String(
				"<!-- components rendered in " & Computed_Modules_Time & " secconds // -->"
			);
		Response := Response & To_Unbounded_String(
				"<!-- components + page rendered in " & Computed_Time & " secconds // -->"
			);
	end Process_Footer;



	overriding
	procedure Finalize_Request(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set
		) is
		-- Finalize processing the request.
		-- Called when the process has been finalized
	begin
		null;
	end Finalize_Request;



	procedure Set_Template(
			Module		: in out Template_Processor_Module;
			Template_Name	: in     Unbounded_String
		) is
		-- Load a template configuration and prepare for processing
	begin
		Module.Template_Descriptor := Templates_Registry.Registry.Get( Template_Name );
		Module.Template_File_Name := To_Unbounded_String(
			Locate_Theme_Resource(
				Component_Name	=> To_String( Module.Component_Name ),
				Theme_Name	=> To_String( Module.Theme_Name ),
				Resource	=> To_String( Template_Name ),
				Extension	=> To_String( Module.Template_Extension )
			)
		);
	end Set_Template;



	function Get_Regions( Module : in Template_Processor_Module ) return KOW_Lib.UString_Vectors.Vector is
	begin
		return Module.Template_Descriptor.Regions;
	end Get_Regions;



	procedure Generic_Append(
			To_Map		: in out Tag_Maps.Map;
			Region		: in     Unbounded_String;
			Contents	: in     String
		) is
		A_Tag: Tag;
	begin
		if Tag_Maps.Contains( To_Map, Region ) then
			A_Tag := Tag_Maps.Element( To_Map, Region );
		end if;

		A_Tag := A_Tag & Contents;

		Tag_Maps.Include( To_Map, Region, A_Tag );
		-- Notice: include replaces or insert a new element
	end Generic_Append;



	procedure Append_Header(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is

		Str_Contents: String := To_String( Contents );
	begin
		Generic_Append(
			To_Map		=> Module.Module_Header_Contents,
			Region		=> Region,
			Contents	=> Str_Contents
		);

		Module.Header_Contents := Module.Header_Contents & Str_Contents;
	end Append_Header;



	procedure Append_Contents(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is
		use Ada.Strings.Fixed;
	begin
		Generic_Append(
			To_Map		=> Module.Module_Contents,
			Region		=> Region,
			Contents	=> To_String( Contents )
		);

		Generic_Append(
			To_Map		=> Module.Module_Ids,
			Region		=> Region,
			Contents	=> Trim( Integer'Image( Index ), Ada.Strings.Both )
		);
	end Append_Contents;



	procedure Append_Footer(
			Module		: in out Template_Processor_Module;
			Region		: in     Unbounded_String;
			Index		: in     Integer;
			Contents	: in     Unbounded_String
		) is
		Str_Contents : String := To_String( Contents );
	begin
		Generic_Append(
			To_Map		=> Module.Module_Footer_Contents,
			Region		=> Region,
			Contents	=> Str_Contents
		);

		Module.Footer_Contents := Module.Footer_Contents & Str_Contents;
	end Append_Footer;


	
	procedure Get_Response(
			Module		: in out Template_Processor_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- Process all the regular module render operations returning the
		-- content rendered.
		--
		-- Notice: Initialize_Request and Finalize_Request should be called elsewhere
	begin
		Process_Header(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters,
			Response	=> Response
		);
		Process_Request(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters,
			Response	=> Response
		);
		Process_Footer(
			Module		=> Module,
			Request		=> Request,
			Parameters	=> Parameters,
			Response	=> Response
		);
	end Get_Response;


