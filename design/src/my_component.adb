

with My_Component.Pages;


with KOW_View.Request_Dispatchers;	use KOW_View.Request_Dispatcher;




package body My_Component is
	Self : Component_Ptr := New_Component( "my_component" );
	-- we are declaring this package as a component
	-- the component name is case-sensitive and should always be lowercase
	--
	-- doing this the component will be registered within the KOW View packages and it will
	-- be possible to query all available services and modules within this component
	--
	-- this is particularly usefull for applications with user settings that should load
	-- the settings page for each service and module
	



	procedure Load is 
		-- this procedure is mandatory
		-- this is where you should load all your component code
	begin

		Dispatch(
				New_Regexp_Dispatcher(
						Expression	=> "^\/\w\/\w\/\*",
						Factory		=> Services.Echo_Factories.Factory
					)
				);


		Dispatch(
				New_Page(
						Expression	=> "/\*",
						Specification	=> Pages.My_Page
					)
			);
				
	end Load;
end My_Component;
