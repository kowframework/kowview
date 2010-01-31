
with KOW_Ent;
with KOW_Ent.Extra_Properties;	use KOW_Ent.Extra_Properties;



package @_application_@.Entities.@_entity_@_@_property_@_hlp is


	function Getter( E : in KOW_Ent.Entity_Type'Class ) return Percent;

	procedure Setter( E : in out KOW_Ent.Entity_Type'Class; Value : in Percent );


end @_application_@.Entities.@_entity_@_@_property_@_hlp;
