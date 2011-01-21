

--------------
-- Ada 2005 --
--------------
with Ada.Command_Line;		use Ada.Command_Line;
with Ada.Text_IO;		use Ada.Text_IO;

-------------------
-- KOW Framework --
-------------------
with KOW_Sec;			use KOW_Sec;



procedure @_project_name_@_Userstatus is

	Data : User_Data_Type;
begin
	if Argument_Count not in 1 .. 2 then
		Put_Line( "usage:" );
		Put_line( Command_Name & " user_identity [new_status]" );
		Put_Line( "Where new status, when set, will be the new user status and can be one of:" );
		for i in Account_Status_Type'Range loop
			Put_line( "   " & Account_Status_Type'Image( i ) );
		end loop;
		return;
	end if;

	Data := Get_user( Argument( 1 ) );

	Put_line( "Current status is " & Account_Status_Type'Image( Data.Account_Status ) );

	if Argument_Count = 2 then
		Data.Account_Status := Account_Status_Type'Value( Argument( 2 ) );
		Store_User( Data );
		Put_line( "New Status set" );
	end if;
end @_project_name_@_Userstatus;
