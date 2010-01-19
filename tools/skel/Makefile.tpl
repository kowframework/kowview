# Makefile for the KOW Web Framework
#
# @author Marcelo Coraça de Freitas <marcelo@kow.com.br> 

PROJECT_FILES=@_lower_project_name_@.gpr
GPR_FILES=@_lower_project_name_@.gpr
INCLUDE_FILES=src/* include/src*


include Makefile.include


pre_libs:

pos_libs:

extra_clean:



run:
	mkdir -p /tmp/aws-@_lower_project_name_@
	./bin/@_lower_project_name_@_server


create_tables:
	./bin/@_lower_project_name_@_create_tables

dump_create_tables:
	./bin/@_lower_project_name_@_dump_create_tables



demouseradd:
	./bin/@_lower_project_name_@_useradd adminuser admin
	./bin/@_lower_project_name_@_groupadd adminuser admin

