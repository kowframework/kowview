


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





procedure @_project_name_@_useradd is
	The_User	: KOW_Sec.User_Data_Type;
	Groups		: KOW_Sec.Group_Vectors.Vector;
begin

	if Argument_Count /= 2 then
		Put_Line( "Usage : " );
		Put_line( Command_Name & " user_identity group" );
		return;
	end if;


	@_project_name_@_Setup;


	The_User	:= KOW_Sec.Get_User( Argument( 1 ) );
	Groups		:= KOW_Sec.Get_Groups( The_User );

	declare
		use KOW_Sec;
		The_Group : constant Group_Type := To_Group( Argument( 2 ) );
	begin
		if Group_Vectors.Contains( Groups, The_Group ) then
			raise PROGRAM_ERROR with "user already in group";
		end if;

		Group_Vectors.Append( Groups, The_Group );
		
		KOW_Sec.Set_Groups(
				User	=> The_User,
				Groups	=> Groups
			);
	end;




end @_project_name_@_useradd;
