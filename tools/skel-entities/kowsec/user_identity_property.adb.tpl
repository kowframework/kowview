

with KOW_Ent;
with KOW_Sec;			use KOW_Sec;



package body @_application_@.Entities.@_entity_@_@_property_@_hlp is


	function Getter( E : in KOW_Ent.Entity_Type'Class ) return User_Identity_Type is
	begin
		return @_entity_@( E ).@_property_@;
	end Getter;

	procedure Setter( E : in out KOW_Ent.Entity_Type'Class; Value : in User_Identity_Type ) is
	begin
		@_entity_@( E ).@_property_@ := Value;
	end Setter;


end @_application_@.Entities.@_entity_@_@_property_@_hlp;
