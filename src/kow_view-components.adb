



--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_View.Components_Registry;



package body KOW_View.Components is

	function Locate_Resource(
			Component	: in Component_Interface;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return KOW_View.Components_Registry.Locate_Resource(
					Component_Name	=> To_String( Component.Component_Name ),
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);

	end Locate_Resource;


	function Locate_Resource(
			Module		: in Module_Instance_Interface;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return Locate_Resource(
					Component	=> Module.Component.all,
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);
	end Locate_Resource;



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




	function Locate_Resource(
			Service		: in Service_Instance_Interface;
			Resource	: in String;
			Extension	: in String := "";
			Kind		: in Ada.Directories.File_Kind := Ada.Directories.Ordinary_File
		) return String is
	begin
		return Locate_Resource(
					Component	=> Service.Component.all,
					Resource	=> Resource,
					Extension	=> Extension,
					Kind		=> Kind
				);
	end Locate_Resource;


end KOW_View.Components;
