


--------------
-- Ada 2005 --
--------------
with Ada.Command_Line;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Lib.String_Util;





package body KOW_View_Tools is


	function Project_Name return String is
	begin
		return Ada.Command_Line.Argument( 2 );
	end Project_Name;



	function Destination_Path( Name : in String ) return String is
		First : Integer := Length( Skel_Path ) + Name'First;

		-- TMP1 and TMP2 variables are needed because of a bug in the
		-- Str_Replace implementation.
		-- The Str parameter must be indexed from 1 to 'Length....
		Tmp1 : String := Name( First .. Name'Last );
		Tmp2 : String( 1 .. Tmp1'Length ) := Tmp1;
		Ret : String := To_String( 
				KOW_Lib.String_Util.Str_Replace(
						From	=> "project_name",
						To	=> Project_Name,
						Str	=> Tmp2
					)
				);
	begin
		if Ret( Ret'First ) = '/' then
			return Ret( Ret'First + 1 .. Ret'Last );
		else
			return Ret;
		end if;
	end Destination_Path;

	function Destination_Path( Name : in Unbounded_String ) return String is
	begin
		return Destination_Path( To_String( Name ) );
	end Destination_Path;


end KOW_View_Tools;
