

with KOW_View.Components_Registry;
procedure KOW_View.Pages.Load is

begin
	KOW_View.Components_Registry.Register(
		"pages",
		new KOW_View.Pages.Component_Type,
		true
	);


end KOW_View.Pages.Load;
