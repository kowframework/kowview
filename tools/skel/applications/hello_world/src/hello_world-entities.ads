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

package Hello_World.Entities is

	type Hello_Entity is new KOW_ent.Entity_Type with record
		Message	: Unbounded_String;
	end record;



end Hello_World.Entities;
