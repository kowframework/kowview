


with @_application_package_@.Entities;		use @_application_package_@.Entities;



with KOW_Ent;




procedure @_application_package_@.Entities_Setup is

	@@TABLE@@
		function The_@_entities_@_Factory return KOW_Ent.Entity_Type'Class is
			E : @_application_package_@.Entities.@_entities_@;
		begin
			return E;
		end My_Factory;
	@@END_TABLE@@
begin


	@@TABLE@@

		KOW_Ent.Entity_Registry.Register(
				Entity_Tag	=> @_application_package_@.Entities.@_entities_@'Tag,
				Table_Name	=> "@_table_names_@",
				Id_Generator	=> @_id_generators_@,
				Factory		=> The_@_entities_@_Factory'Access
			);
	
		
		@@TABLE@@
	
		@_properties_@
	
		@@END_TABLE@@
	
	@@END_TABLE@@

end @_application_package_@.Entities_Setup;
