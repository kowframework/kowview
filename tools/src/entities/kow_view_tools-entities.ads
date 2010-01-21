
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;


package KOW_View_Tools.Entities is

	type Property_Descriptor_Type is abstract tagged record
		Column_Name	: Unbounded_String;
	end record;


--	function Get_Declaration( P : in Property_Descriptor_Type ) return String is abstract;


	type Entity_Descriptor_Type is tagged record
		Table_name	: Unbounded_String;
	end record;

--	function Get_Declaration( P : in Entity_Descriptor_Type ) return String is abstract;

--	function Get_Registration_Code( P : in Entity_Descriptor_Type ) return String;
--
--	function Get_Implementation_Code( P : in Entity_Descriptor_Type ) return String;
	
	
	function Process_Entities( App : Unbounded_String ) return Boolean;

end KOW_View_Tools.Entities;
