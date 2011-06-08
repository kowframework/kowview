/**
 * Some important functions for modules. :)
 */


kowview.services = {

	jsonURL		: function( component, service, localUri) {
				url = '/' + component + '/' + service;
				if( kowview.isSet( localUri ) )
					url += '/' + localUri;
				url += "?mode=json";

				return url;
			},

	postJson	: function ( component, service, parameters, localUri ) {
				kowview.postJson( parameters, this.jsonURL( component, service, localUri ) );
			},
	getJson		: function ( component, service, parameters, localUri ) {
				kowview.getJson( parameters, this.jsonURL( component, service, localUri ) );
			},
	iframeSend	: function ( component, service, parameters, localUrl ) {
				kowview.iframeSend( paramters, this.jsonURL( component, service, localUri ) );
			}

}

