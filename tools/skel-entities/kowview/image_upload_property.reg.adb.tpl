	KOW_Ent.Entity_Registry.Add_Property(
			Entity_Tag	=> @_application_@.entities.@_entity_@'Tag,
			Property	=> KOW_View.Entities.Properties.New_Image_Upload_Property(
							Column_Name	=> "@_column_name_@",
							Getter		=> @_getter_@'Unrestricted_Access,
							Setter		=> @_setter_@'Unrestricted_Access,
							Upload_Path	=> "@_upload_path_@"
							@@IF@@ @_default_value_@ /= ""
								,Default_Value	=> "@_default_value_@"
							@@END_IF@@
							@@IF@@ @_immutable_@ /= ""
								,Immutable	=> @_immutable_@
							@@END_IF@@
							@@IF@@ @_length_@ /=""
								,Length		=> @_length_@
							@@END_IF@@
							@@IF@@ @_thumbnail_@ /=""
								,Thumbnail	=> "@_thumbnail_@"
							@@END_IF@@
							@@IF@@ @_big_@ /=""
								,Big		=> "@_big_@"
							@@END_IF@@

							@@IF@@ @_convert_@ /= ""
								,Convert	=> "@_convert_@"
							@@END_IF@@
						)
			@@IF@@ @_is_unique_@ /= ""
				,Is_Unique => @_is_unique_@
			@@END_IF@@
		);

