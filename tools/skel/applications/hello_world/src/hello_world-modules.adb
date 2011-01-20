


with KOW_View.Locales;
with KOW_View.Modules;
with KOW_View.Modules.Stateful_Module_Factories;


with Hello_World.Components;

with Ada.Strings.Unbounded;				use Ada.Strings.Unbounded;

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


		Insert( Parameters, Assoc( "counter", Natural'Image( Module.Counter ) ) );

		Response := Parse_Template(
						Module			=> Module,
						Template_Resource	=> "the_view",
						Template_Extension	=> "html",
						Parameters		=> Parameters,
						Locale			=> KOW_View.Locales.Get_Locale( Request )
				);
	end Process_Body;


	protected body incrementor is
		procedure increment( C : in out natural ) is
		begin
			C := C + 1;
		end increment;
	end incrementor;

end Hello_World.Modules;
