# Makefile for the KOW Web Framework
#
# @author Marcelo Coraça de Freitas <marcelo@kow.com.br> 

PROJECT_FILES=@_lower_project_name_@.gpr
GPR_FILES=@_lower_project_name_@.gpr
INCLUDE_FILES=src/* include/src*


include Makefile.include


pre_libs:

pos_libs:
	make -C tools

extra_clean:
