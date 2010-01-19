



with "apq.gpr";
with "apq_provider.gpr";
with "apq_provider-mysql.gpr";

with "kowconfig.gpr";
with "kowconfig-text.gpr";
with "kowent.gpr";
with "kowlib.gpr";
with "kowsec.gpr";
with "kowsec-entities.gpr";
with "kowview.gpr";
with "kowview-entities.gpr";

with "aws";


project @_project_name_@ is

	Version := $version;
	
	
	for Source_Dirs use ( "src", "applications/**" );
	
	for Object_Dir use "obj";
	
	for Exec_Dir use "bin";
	for Main use ( "@_lower_project_name_@_server" );
	
	package Linker renames KOWLib.Linker;



end @_project_name_@;
