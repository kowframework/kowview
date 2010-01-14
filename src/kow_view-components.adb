

--------------
-- Ada 2005 --
--------------
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;





package body KOW_View.Components is

	procedure Generate_HTML_ID(
				Module		: in out Module_Instance_Interface;
				The_ID		:    out Unbounded_String
		) is
		-- procedure used to generate a valid ID for HTML elements
		-- it's a helper procedure so the user can produce unique IDs for their pages easily

		function T( N : in Natural ) return String is
			use Ada.Strings, Ada.Strings.Fixed;
		begin
			return Trim( Natural'Image( N ), Both );
		end T;

	begin
		Module.ID_Count := Module.ID_Count + 1;

		The_ID := To_Unbounded_String( "module_" & T( Natural( Module.Module_ID ) ) & "_id_" & T( Module.ID_Count ) );
				
	end Generate_HTML_ID;
end KOW_View.Components;
