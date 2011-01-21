/**
 * Some important functions for modules. :)
 */


kowview.services = {

	buildParams	: function( parameters ) {
				param = new Object();

				param.form = parameters.form;

				theHref = '/' + parameters.component + '/' + parameters.service + "?mode=json";
				console.log( theHref );

				param.url = theHref;
				param.handleAs = 'json';

				param.load = function( responseObject, ioArgs ) {
						parameters.load( responseObject );
					};
				param.error = function( data ) {
						parameters.error( dojo.fromJson( data.responseText ) );
					};

				return param;
			},

	postJson	: function ( parameters ) {
				dojo.xhrPost( this.buildParams( parameters ) );
			},
	getJson		: function ( parameters ) {
				dojo.xhrGet( this.buildParams( parameters ) );
			}

}

