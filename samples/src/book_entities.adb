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

with Ada.Text_IO;	use Ada.Text_IO;

---------
-- APQ --
---------

with APQ;


------------
-- Aw_Ent --
------------

with Aw_Ent;
with Aw_Ent.Properties;

package body Book_Entities is


	overriding
	function To_String( Entity : in Book_Type ) return String is
	begin
		return Ada.Strings.Unbounded.To_String( Entity.Title );
	end To_String;



	procedure Set_Author_Foreign_Key( Entity : in out Book_Type; Key_From : Aw_Ent.Entity_Type'Class ) is
	begin
		Entity.Author_ID := Key_From.ID;
	end Set_Author_Foreign_Key;


	overriding
	function To_String( Entity : in Author_Type ) return String is
	begin
		return Ada.Strings.Unbounded.To_String( Entity.Name );
	end To_String;




	--
	-- Getter and setter for the book properties
	--
	procedure Set_Title( Book : in out Aw_Ent.Entity_Type'Class; Title : in Unbounded_String ) is
	begin
		Book_Type(Book).Title := Title;
	end Set_Title;


	function Get_Title( Book : in Aw_Ent.Entity_Type'Class ) return Unbounded_String is
	begin
		return Book_Type(Book).Title;
	end Get_Title;

	procedure Set_Extra_Info( Book : in out Aw_Ent.Entity_Type'Class; Extra_Info : in Unbounded_String ) is
	begin
		Book_Type(Book).Extra_Info := Extra_Info;
	end Set_Extra_Info;

	function Get_Extra_Info( Book : in Aw_Ent.Entity_Type'Class ) return Unbounded_String is
	begin
		return Book_Type(Book).Extra_Info;
	end Get_Extra_Info;


	procedure Set_Author_ID( Book : in out Aw_Ent.Entity_Type'Class; ID : in Aw_Ent.ID_Type ) is
	begin
		Book_Type(Book).Author_ID := ID;
	end Set_Author_ID;


	function Get_Author_ID( Book : in Aw_Ent.Entity_Type'Class ) return Aw_Ent.ID_Type is
	begin
		return Book_Type(Book).Author_ID;
	end Get_Author_ID;

	--
	-- Getter and setter for the author properties
	--
	procedure Set_Name( Author : in out Aw_Ent.Entity_Type'Class; Name : in Unbounded_String ) is
	begin
		Author_Type( Author ).Name := Name;
	end Set_Name;

	function Get_Name( Author : in Aw_Ent.Entity_Type'Class ) return Unbounded_String is
	begin
		return Author_Type( Author ).Name;
	end Get_Name;



	-- 
	-- Some useful procedures
	--
	procedure Put( Book : in Book_Type ) is
	begin
		Put_Line( "Book #" & APQ.APQ_Bigserial'Image( Book.id.value ) );
		Put     ( "Title  ......... " );
		Put_Line( To_String( Book.Title ) );
		Put_Line( "Other info ..... " );
		Put_Line( To_String( Book.Extra_Info ) );
	end Put;


	function Author_Factory return Aw_Ent.Entity_Type'Class is
		Ent : Author_Type;
	begin
		return Ent;
	end Author_Factory;

	function Book_Factory return Aw_Ent.Entity_Type'Class is
		Ent : Book_Type;
	begin
		return Ent;
	end Book_Factory;

begin



	--
	-- Now we need to register the entities so Aw_Ent will know how to handle then
	--
	

	-- AUTHOR ::
	Aw_Ent.Entity_Registry.Register(
		Entity_Tag	=> Author_Type'Tag,
		Table_Name	=> "authors",
		Id_Generator	=> Null,
		Factory		=> Author_Factory'Access
		);
	
	Aw_Ent.Entity_Registry.Add_Property(
		Entity_Tag	=> Author_Type'Tag,
		Property	=> Aw_Ent.Properties.New_UString_Property(
						Column_Name	=> "name",
						Getter		=> Get_Name'Access,
						Setter		=> Set_Name'Access
					)
		);

	-- BOOK ::
	Aw_Ent.Entity_Registry.Register(
		Entity_Tag	=> Book_Type'Tag,
		Table_Name	=> "books",
		Id_Generator	=> Null,
		Factory		=> Book_Factory'Access
		);

	Aw_Ent.Entity_Registry.Add_Property(
		Entity_Tag	=> Book_Type'Tag,
		Property	=> Aw_Ent.Properties.New_UString_Property(
						Column_Name	=> "title",
						Getter		=> Get_Title'Access,
						Setter		=> Set_Title'Access
					)
		);

	Aw_Ent.Entity_Registry.Add_Property(
		Entity_Tag	=> Book_Type'Tag,
		Property	=> Aw_Ent.Properties.New_UString_Property(
						Column_Name	=> "extra_info",
						Getter		=> Get_Extra_Info'Access,
						Setter		=> Set_Extra_Info'Access
					)
		);
	Aw_Ent.Entity_Registry.Add_Property(
		Entity_Tag	=> Book_Type'Tag,
		Property	=> Aw_Ent.Properties.New_Foreign_Key_Property(
						Column_Name		=> "author_id",
						Related_Entity_Tag	=> Author_Type'Tag,
						Getter			=> Get_Author_ID'Access,
						Setter			=> Set_Author_ID'Access
					)
		);
	
end Book_Entities;
