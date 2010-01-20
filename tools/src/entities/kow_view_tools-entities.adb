




--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;



-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Text;
with KOW_Lib.File_System;

package body KOW_View_Tools.Entities is


	function Process_Entities( Application : Unbounded_String ) return Boolean is
		Cfg : KOW_Config.Config_File;

		Sep	: constant Character := KOW_Lib.File_System.Separator;

		App_Str : String := To_String( Application );

		App_Path : String := "applications" & Sep & App_Str & Sep;
		
		App_Cfg : String := App_Path & "application.cfg";
		Ent_Cfg : String := App_Path & "entities.cfg";


		The_Parser : aliased KOW_Config.Text.Parser;
	begin

		Cfg := KOW_Config.New_Config_File(
						N	=> App_Cfg,
						P	=> The_Parser'Unchecked_Access
				);

		if KOW_Config.Value( Cfg, "process_entities", False ) then
			Cfg := KOW_Config.New_Config_File(
						N	=> Ent_Cfg,
						P	=> The_Parser'Unchecked_Access
				);
		end if;


		return true;
	exception
		when KOW_Config.FILE_NOT_FOUND =>
			return false;
	end process_entities;

end KOW_View_Tools.Entities;
