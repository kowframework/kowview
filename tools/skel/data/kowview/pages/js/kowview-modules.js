/**
 * Some important functions for modules. :)
 */


kowview.modules = {


	jsonURL		: function( module_id ) {
				url = document.location.href;
				if( url.indexOf( "?" ) > -1 )
					url += "&";
				else
					url += "?";
				url += "mode=json&module_id="+module_id;

				return url;
			},

	postJson	: function ( module_id, parameters ) {
				kowview.postJson( parameters, kowview.modules.jsonURL( module_id ) );
			},
	getJson		: function ( module_id, parameters ) {
				kowview.getJson( parameters, kowview.modules.jsonURL( module_id ) );
			}

}

