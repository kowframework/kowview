	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> @_application_@.@_entity_@'Tag
			Property	=> KOW_Ent.Properties.New_UString_Property(
							Column_Name	=> @_column_name_@,
							Getter		=> @_getter_@'Access,
							Setter		=> @_setter_@'Access
						)
		);

