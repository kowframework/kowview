-- This is the package where I declare the entities...
--
--




--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Ent;
with KOW_Lib.Locales;

package Hello_World.Entities is

	type Hello_Entity is new KOW_ent.Entity_Type with record
		Message		: Unbounded_String;
		Message_Locale	: KOW_Lib.Locales.Locale;
		Password	: Unbounded_String;
		Boolvalue	: Boolean;
	end record;

	type FK_Test_Entity is new KOW_Ent.Entity_Type with record
		Hello_ID	: KOW_Ent.ID_Type;
	end record;



end Hello_World.Entities;
