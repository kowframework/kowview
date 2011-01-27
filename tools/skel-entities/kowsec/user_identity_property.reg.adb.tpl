	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> @_application_@.entities.@_entity_@'Tag,
			Property	=> KOW_Sec.Entities.New_User_Identity_Property(
							Column_Name	=> "@_column_name_@",
							Getter		=> @_getter_@'Unrestricted_Access,
							Setter		=> @_setter_@'Unrestricted_Access
							@@IF@@ @_default_value_@ /= ""
								,Default_Value	=> "@_default_value_@"
							@@END_IF@@
							@@IF@@ @_immutable_@ /= ""
								,Immutable	=> @_immutable_@
							@@END_IF@@
						)
			@@IF@@ @_is_unique_@ /= ""
				,Is_Unique => @_is_unique_@
			@@END_IF@@
		);

