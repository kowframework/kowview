


--------------
-- Ada 2005 --
--------------
with Ada.Command_Line;			use Ada.Command_Line;
with Ada.Text_IO;			use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Sec.Authentication.Entities;

with @_project_name_@_setup;





procedure @_project_name_@_useradd is
	A_User : KOW_Sec.Authentication.Entities.User_Type;
begin

	if Argument_Count /= 2 then
		Put_Line( "Usage : " );
		Put_line( Command_Name & " user_identity group" );
		return;
	end if;


	@_project_name_@_Setup;

	KOW_Sec.Authentication.Entities.Add_Group(
			User_Identity	=> Argument( 1 ),
			Group		=> Argument( 2 )
		);

end @_project_name_@_useradd;