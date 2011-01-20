/**
 * Some important functions for modules. :)
 */


kowview.modules = {

	buildParams	: function( module_id, parameters ) {
				param = new Object();

				param.form = parameters.form;

				theHref = document.location.href;
				if( theHref.indexOf( "?" ) > -1 )
					theHref += "&";
				else
					theHref += "?";
				theHref += "mode=json&module_id="+module_id;
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

	postJson	: function ( module_id, parameters ) {
				dojo.xhrPost( this.buildParams( module_id, parameters ) );
			},
	getJson		: function ( module_id, parameters ) {
				dojo.xhrGet( this.buildParams( module_id, parameters ) );
			}

}

