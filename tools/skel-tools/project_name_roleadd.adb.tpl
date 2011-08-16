


--------------
-- Ada 2005 --
--------------
with Ada.Command_Line;			use Ada.Command_Line;
with Ada.Text_IO;			use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Sec;

with @_project_name_@_setup;





procedure @_project_name_@_roleadd is
	The_User	: KOW_Sec.User_Data_Type;
begin

	if Argument_Count /= 2 and then Argument_Count /= 3 then
		Put_Line( "Usage : " );
		Put_line( Command_Name & " user_identity role" );
		return;
	end if;


	@_project_name_@_Setup;


	The_User	:= KOW_Sec.Get_User( Argument( 1 ) );
	
	declare
		The_Role : constant KOW_Sec.Role_Type := KOW_Sec.To_Role( KOW_Sec.to_Identity( Argument( 2 ) ) );
	begin
		KOW_Sec.Add_Global_Role( The_User, The_Role );
	end;




end @_project_name_@_roleadd;
