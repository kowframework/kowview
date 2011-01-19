with Ada.Text_IO;		use Ada.Text_IO;



with Hello_World.Components;
with Hello_World.Services;
pragma Elaborate( Hello_World.Services );
with Hello_World.Modules;
pragma Elaborate( Hello_World.Modules );



with KOW_View.Components.Registry;


procedure Hello_World.Load is
begin
	KOW_View.Components.Registry.Register(
			Hello_World.Components.Component'Access,
			false
		);
	Ada.Text_IO.Put_Line( "Hello World loaded!" );
end Hello_World.Load;
