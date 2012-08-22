


with KOW_View.Pages;

with My_Component.Modules;



package My_Component.Pages is


	type Regions is (
			Menu_Bar,
			Diplay_Bar,
			Content_Bar
		);

	package Base is new KOW_View.Pages.Base( Regions );
	-- this will declare the page_type for specific set of regions within the page
	--
	-- enforcing strong typing baby!




	type My_Page is new Base.Page_Type( Title( "My Pretty Little Page" ) )  with null record;



	overriding
	function Allowed(
				Page		: in My_Page;
				Status		: in KOW_View.Request_Status_Type
			) return Boolean;
	-- check if the current page is allowed


	overriding
	function Get_Modules(
				Page		: in My_Page;
				Status		: in KOW_View.Request_Status_Type;
				Region		: in Regions
			) return KOW_View.Pages.Module_Array;
	-- get the module factory array for this specific page





	type My_Secure_Page is new Base.Secure_page(
					Title( "My Profile" ),
					To_Criteria( "USERNAME/=ANONYMOUS" )
				);

end My_Component.Pages;
