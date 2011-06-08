


function submit_login_form( module_id ) {
	theForm = dojo.byId( "login_form_" + module_id );
	kowview.services.postJson(
					"security",
					"login",
					{
						form		: theForm,
						load		: function( responseObject ) {
									console.dir( responseObject )
									document.location.href="/";
								},
						error		: function( data ) {
									console.dir( data );
									kowview.showErrorMessage( "Login Error", data.message );
								}
					}
			);
}




