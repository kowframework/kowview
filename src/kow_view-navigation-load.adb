
with KOW_View.COmponents_Registry;



procedure KOW_View.Navigation.Load is

begin

	KOW_View.Components_Registry.Register(
		"navigation",
		new KOW_View.Navigation.Component_Type,
		true
	);

end KOW_View.Navigation.Load;
