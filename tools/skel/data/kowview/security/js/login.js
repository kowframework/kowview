


function submit_login_form( module_id ) {
	theForm = dojo.byId( "login_form_" + module_id );
	console.log("vou fz login... eeee " + module_id );

	kowview.services.postJson({
			component	: 'security',
			service		: 'login',
			form		: theForm,
			load		: function( responseObject ) {
						console.dir( responseObject )
						document.location.reload();
					},
			error		: function( data ) {
						console.dir( data );
						kowview.errorMessage( "Login Form", data.message );
					}
		});
}




