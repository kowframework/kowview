

with KOW_Ent;
with KOW_Ent.Extra_Properties;	use KOW_Ent.Extra_Properties;



package body @_application_@.Entities.@_entity_@_@_property_@_hlp is


	function Getter( E : in KOW_Ent.Entity_Type'Class ) return Percent is
	begin
		return @_entity_@( E ).@_property_@;
	end Getter;

	procedure Setter( E : in out KOW_Ent.Entity_Type'Class; Value : in Percent ) is
	begin
		@_entity_@( E ).@_property_@ := Value;
	end Setter;


end @_application_@.Entities.@_entity_@_@_property_@_hlp;
