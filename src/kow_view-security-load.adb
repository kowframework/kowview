with KOW_View.Components_Registry;



procedure KOW_View.Security.Load is
begin
	KOW_View.Components_Registry.Register(
			Component_Name		=> "security",
			Component		=> new KOW_View.Security.Component_Type,
			Require_Configuration	=> true
		);

end KOW_View.Security.Load;
