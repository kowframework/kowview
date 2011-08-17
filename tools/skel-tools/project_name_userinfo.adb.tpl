

--------------
-- Ada 2005 --
--------------
with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Text_IO;		use Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Lib;
with KOW_Lib.Locales;
with KOW_Sec;			use KOW_Sec;
with KOW_Sec.Entities;


with @_project_name_@_setup;

procedure @_project_name_@_Userinfo is


	Data : User_Data_Type;

	Indent_Level : Positive := 2;

	procedure Roles_Iterator( C : Role_Vectors.Cursor ) is
	begin
		for i in 1 .. Indent_level loop
			Put( ' ' );
		end loop;

		Put_Line( " => " & String( Identity( Role_Vectors.Element( C ) ) ) );
	end Roles_Iterator;


	procedure Groups_iterator( C : Group_Vectors.Cursor ) is
	begin
		Put_Line( "  => " & To_String( Group_Vectors.Element( C ) ) );
		Role_Vectors.Iterate( Get_Roles( Group_Vectors.Element( C ) ), Roles_Iterator'Access );
	end Groups_Iterator;
begin
	if Argument_Count not in 1 .. 2 then
		Put_Line( "usage:" );
		Put_line( Command_Name & " user_identity" );
		Put_Line( "or" );
		Put_line( Command_Name & " -u username" );
	end if;

	if Argument( 1 ) = "-u" then
		if Argument_Count /= 2 then
			raise CONSTRAINT_ERROR with "-u option requires the username after";
		end if;
		@_project_name_@_Setup( False );
		Data := Get_User( KOW_Sec.Entities.Get_User_Entity( Username => Argument( 2 ) ).User_Identity );
	else
		Data := Get_user( Argument( 1 ) );
	end if;

	Put_Line( "Current info is " );

	Put_Line( "TODO:: IMPLEMENT THE CORE USER INFO FEATURE" );


	Put_Line( "Roles:" );
	Role_Vectors.Iterate( Get_Roles( Data, False ), Roles_Iterator'Access );

	Indent_Level := 4;
	Put_line( "Groups:" );
	Group_Vectors.Iterate( Get_All_Groups( Data ), Groups_iterator'Access );


end @_project_name_@_Userinfo;
