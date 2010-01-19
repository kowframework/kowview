



-------------------
-- KOW Framework --
-------------------
with KOW_Ent;

-------------
-- KOW ERP --
-------------
with @_project_name_@_Setup;





procedure @_project_name_@_Create_Tables is
begin
	@_project_name_@_Setup;

	KOW_Ent.Run_Create( True );

end @_project_name_@_Create_Tables;
