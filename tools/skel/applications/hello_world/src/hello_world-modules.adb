



with KOW_View.Modules;
with KOW_View.Modules.Stateful_Module_Factories;


with Hello_World.Components;

with Ada.Strings.Unbounded;				use Ada.Strings.Unbounded;
with AWS.Status;


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
		Buffer : Unbounded_String;
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


		Append( Buffer, "<div dojoType=""dijit.TitlePane"" title=""Hello there!"">" );
			Append( Buffer, "I have been acessed " );
			Append( Buffer, natural'image( Module.Counter ) );
			Append( Buffer, " times. But also check my singleton service as <a href=""/hello_world/hello"">HTML</a>" );
			Append( Buffer, " and <a href=""/hello_world/hello?mode=json"">JSON</a>." );
		Append( Buffer, "</div>" );

		Response := Buffer;
	end Process_Body;


	protected body incrementor is
		procedure increment( C : in out natural ) is
		begin
			C := C + 1;
		end increment;
	end incrementor;

end Hello_World.Modules;
