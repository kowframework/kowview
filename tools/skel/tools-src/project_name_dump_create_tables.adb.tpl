



--------------
-- Ada 2005 --
--------------
with Ada.Text_IO;		use Ada.Text_IO;
-------------------
-- KOW Framework --
-------------------
with KOW_Ent;

with @_project_name_@_Setup;





procedure @_project_name_@_Dump_Create_Tables is
begin
	@_project_name_@_Setup;

	New_Line( 10 );

	Put_Line( KOW_Ent.Get_Create( True ) );

end @_project_name_@_Dump_Create_Tables;
