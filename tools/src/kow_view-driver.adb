
with Ada.Command_Line;
with Ada.Text_IO;



package body KOW_View.Driver is

	procedure Print_Usage is
		use Ada.Command_Line;
		use Ada.Text_IO;
	begin
		Put_Line( "usage: " & Command_Name & " command [parameters]" );
		Put_Line( "Available commands: " );
		Put_Line( "    create    => create a new kow_view project." );
	end Print_Usage;
end KOW_View.Driver;
