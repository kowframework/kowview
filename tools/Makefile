# Makefile for the KOW Web Framework
#
# @author Marcelo Coraça de Freitas <marcelo@kow.com.br> 

PROJECT_FILES=kowview_tools.gpr
GPR_FILES=
INCLUDE_FILES=src/*


include Makefile.include


pre_libs:
	gnatprep "-Dversion=\"$(VERSION)\"" src/kow_view-version.ads.in src/kow_view-version-.ads

pos_libs:

extra_clean:
