function submit_logout() {
	kowview.services.postJson({
			component	: 'security',
			service		: 'logout',
			load		: function( responseObject ) {
						console.dir( responseObject )
						document.location.reload();
					},
			error		: function( data ) {
						console.dir( data );
						kowview.errorMessage( "Logout", data.message );
					}
		});

}
