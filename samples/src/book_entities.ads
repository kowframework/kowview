-- This maps some entities for our book store
--
--
--
--
--
--


--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


------------
-- Aw_Ent --
------------
with Aw_Ent;
with Aw_Ent.Query_Builders;
with Aw_Ent.Relations;

package Book_Entities is


	type Book_Type is new Aw_Ent.Entity_Type with record
		Title		: Unbounded_String;
		Extra_Info	: Unbounded_String;
		Author_ID	: Aw_Ent.ID_Type;
	end record;
	
	overriding
	function To_String( Entity : in Book_Type ) return String;

	procedure Set_Author_Foreign_Key( Entity : in out Book_Type; Key_From : in Aw_Ent.Entity_Type'Class );

	type Author_Type is new Aw_Ent.Entity_Type with record
		Name	: Unbounded_String;
	end record;

	overriding
	function To_String( Entity : in Author_Type ) return String;


	--
	-- Getter and setter for the book properties
	--
		
	procedure Set_Title( Book : in out Aw_Ent.Entity_Type'Class; Title : in Unbounded_String );
	function Get_Title( Book : in Aw_Ent.Entity_Type'Class ) return Unbounded_String;

	procedure Set_Extra_Info( Book : in out Aw_Ent.Entity_Type'Class; Extra_Info : in Unbounded_String );
	function Get_Extra_Info( Book : in Aw_Ent.Entity_Type'Class ) return Unbounded_String;


	--
	-- Getter and setter for the author properties
	--
	procedure Set_Name( Author : in out Aw_Ent.Entity_Type'Class; Name : in Unbounded_String );
	function Get_Name( Author : in Aw_Ent.Entity_Type'Class ) return Unbounded_String;

	-- 
	-- Some useful procedures
	--
	procedure Put( Book : in Book_Type );



	package Book_Query is new Aw_Ent.Query_Builders(
				Entity_Type => Book_Type
			);

	package Author_Books_Handlers is new Aw_Ent.Relations.One_To_Many_Relation_Handlers(
				From_Entity_Type	=> Author_Type,
				To_Entity_Type		=> Book_Type,
				Set_Foreign_Key		=> Set_Author_Foreign_Key
			);


end Book_Entities;
