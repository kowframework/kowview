


with KOW_View.Components; with KOW_View.Components;







package My_Component is
	Self : Component_Ptr := New_Component( "my_component" );
	-- we are declaring this package as a component
	-- the component name is case-sensitive and should always be lowercase
	--
	-- doing this the component will be registered within the KOW View packages and it will
	-- be possible to query all available services and modules within this component
	--
	-- this is particularly usefull for applications with user settings that should load
	-- the settings page for each service and module
	



	procedure Load is null;
	-- this procedure is mandatory
	-- this is where you should load all your component code

end My_Component;
