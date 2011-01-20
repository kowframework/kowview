


with KOW_View.Locales;
with KOW_View.Modules;
with KOW_View.Modules.Stateful_Module_Factories;


with Hello_World.Components;

with Ada.Strings.Unbounded;				use Ada.Strings.Unbounded;

with AWS.Parameters;
with AWS.Status;
with Templates_Parser;


package body Hello_World.Modules is


	protected incrementor is
		procedure increment( C : in out natural );
	end incrementor;

	overriding
	procedure Process_Body(
				Module	: in out Hello_There_Module;
				Request	: in     AWS.Status.Data;
				Response:    out Unbounded_String
			) is
		use Templates_Parser;
		Parameters : Translate_Set;
	begin

		Incrementor.Increment( Module.Counter );

		Include_Dojo_Package( Module, "dijit.TitlePane" );
		-- you can include Dojo packages directly from the Ada code...
		-- and they won't be include twice by the JS application...
		
		-- just to show you ...
		Include_Dojo_Package( Module, "dijit.TitlePane" );
		Include_Dojo_Package( Module, "dijit.TitlePane" );
		Include_Dojo_Package( Module, "dijit.TitlePane" );
		-- but please don't do that :)


		Include_Dojo_Package( Module, "dijit.Dialog" );
		Include_Dojo_Package( Module, "dijit.form.Button" );

		Include_Component_Script( Module, "information.js" );
		Include_Component_Script( Module, "updater.js" );

		Include_Component_CSS( Module, "style.css" );


		Insert( Parameters, Assoc( "counter", Module.Counter ) );
		Insert( Parameters, Assoc( "module_id", Get_id( Module ) ) );

		Response := Parse_Template(
						Module			=> Module,
						Template_Resource	=> "the_view",
						Template_Extension	=> "html",
						Parameters		=> Parameters,
						Locale			=> KOW_View.Locales.Get_Locale( Request )
				);
	end Process_Body;


	overriding
	procedure Process_Json_Request(
				Module	: in out Hello_There_Module;
				Request	: in     AWS.Status.Data;
				Response:    out KOW_Lib.Json.Object_Type
			) is
		Object : KOW_Lib.Json.Object_Type;
	begin
		Incrementor.Increment( Module.Counter );
		
		if Module.Counter mod 2 = 0 then
			KOW_Lib.Json.Set( Object, "counter", Module.Counter );
		else
			KOW_Lib.Json.Set( Object, "counter", "<span style=""color:red;"">" & Natural'Image( Module.Counter ) & "</span>");
		end if;
		KOW_Lib.Json.Set( Object, "lala", AWS.Parameters.Get( AWS.Status.Parameters( Request ), "lala" ) );

		Response := Object;
	end Process_Json_Request;



	protected body incrementor is
		procedure increment( C : in out natural ) is
		begin
			C := C + 1;
		end increment;
	end incrementor;

end Hello_World.Modules;
