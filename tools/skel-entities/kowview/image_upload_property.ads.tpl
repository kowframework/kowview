with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


with KOW_Ent;
with KOW_View.Entity_Properties;	use KOW_View.Entity_Properties;



package @_application_@.Entities.@_entity_@_@_property_@_hlp is


	function Getter( E : in KOW_Ent.Entity_Type'Class ) return Unbounded_String;

	procedure Setter( E : in out KOW_Ent.Entity_Type'Class; Value : in Unbounded_String );


end @_application_@.Entities.@_entity_@_@_property_@_hlp;
