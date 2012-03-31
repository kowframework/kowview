



--------------
-- Ada 2005 --
--------------
with Ada.Text_IO;		use Ada.Text_IO;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.Json;		use KOW_Lib.Json;
with KOW_Lib.Log;
with KOW_View.KTML;



procedure KTMLTest is

	State : Object_Type;
	Some_Obj : Object_Type;


	procedure Add_Users is
		Users : Array_Type;

		procedure new_user( username, name: in string ) is
			U : Object_Type;
		begin
			Set( U, "username", username );
			Set( U, "name", name );
			Append( Users, U );
		end new_user;
	begin
		New_User( "ogro", "Marcelo" );
		New_User( "magho", "Marco" );

		Set( State, "users", users );
	end Add_Users;
begin

	Set( Some_Obj, "omg1", "lalala" );
	Set( Some_Obj, "omg2", "lalala2" );
	Set( Some_Obj, "omg3", "lalala3" );

	Set( State, "some_obj", Some_Obj );

	Set( State, "welcome", "Bem vindo!" );
	Add_Users;
	KOW_Lib.Log.Default_Level := KOW_Lib.Log.Level_Debug;
	Put_Line( KOW_View.KTML.Render( "./data/example.ktml", State ) );
end KTMLTest;
