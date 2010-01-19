


with KOW_View.Components_Registry;

procedure KOW_View.Themes.Load is
begin
	KOW_View.Components_Registry.Register(
		"themes",
		new KOW_View.Themes.Component_Type,
		true
	);


end KOW_View.Themes.Load;
