

with KOW_Ent;
with KOW_Ent.Properties;	use KOW_Ent.Properties;
with KOW_Sec;			use KOW_Sec;



package @_application_@.Entities.@_entity_@_@_property_@_hlp is


	function Getter( E : in KOW_Ent.Entity_Type'Class ) return User_Identity_Type;

	procedure Setter( E : in out KOW_Ent.Entity_Type'Class; Value : in User_Identity_Type );


end @_application_@.Entities.@_entity_@_@_property_@_hlp;
