
with KOW_Ent;			use KOW_Ent;
with KOW_Ent.Properties;	use KOW_Ent.Properties;



package @_application_@.Entities.@_entity_@_@_property_@_hlp is


	function Getter( E : in KOW_Ent.Entity_Type'Class ) return ID_Type;

	procedure Setter( E : in out KOW_Ent.Entity_Type'Class; Value : in ID_Type );


end @_application_@.Entities.@_entity_@_@_property_@_hlp;
