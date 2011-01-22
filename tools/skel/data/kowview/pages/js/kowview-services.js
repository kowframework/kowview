/**
 * Some important functions for modules. :)
 */


kowview.services = {

	jsonURL		: function( component, service ) {
				url = '/' + component + '/' + service + "?mode=json";

				return url;
			},

	postJson	: function ( component, service, parameters ) {
				kowview.postJson( parameters, this.jsonURL( component, service ) );
			},
	getJson		: function ( component, service, parameters ) {
				kowview.getJson( parameters, this.jsonURL( component, service ) );
			}

}

