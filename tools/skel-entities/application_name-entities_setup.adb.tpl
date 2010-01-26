


with @_application_@.Entities;		use @_application_@.Entities;



with KOW_Lib.Locales;

with KOW_Ent;
with KOW_Ent.Properties;



-------------------------
-- Getters and Setters --
-------------------------
@@TABLE@@
with @_gs_packages_@;
@@END_TABLE@@

procedure @_application_@.Entities_Setup is

	@@TABLE@@
		function The_@_entities_@_Factory return KOW_Ent.Entity_Type'Class is
			E : @_application_@.Entities.@_entities_@;
		begin
			return E;
		end The_@_entities_@_Factory;
	@@END_TABLE@@
begin


	@@TABLE@@

		KOW_Ent.Entity_Registry.Register(
				Entity_Tag	=> @_application_@.Entities.@_entities_@'Tag,
				Table_Name	=> "@_table_names_@",
				Id_Generator	=> @_id_generators_@,
				Factory		=> The_@_entities_@_Factory'Access
			);
	
		
		@@TABLE@@
	
		@_properties_@
	
		@@END_TABLE@@
	
	@@END_TABLE@@


	null;

end @_application_@.Entities_Setup;
