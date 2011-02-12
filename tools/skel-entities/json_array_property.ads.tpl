with KOW_Ent;
with KOW_Ent.Properties;	use KOW_Ent.Properties;
with KOW_Lib.Json;



package @_application_@.Entities.@_entity_@_@_property_@_hlp is


	function Getter( E : in KOW_Ent.Entity_Type'Class ) return KOW_Lib.Json.Array_Type;

	procedure Setter( E : in out KOW_Ent.Entity_Type'Class; Value : in KOW_Lib.Json.Array_Type );


end @_application_@.Entities.@_entity_@_@_property_@_hlp;
