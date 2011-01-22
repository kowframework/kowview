/**
 * Main package for kowview js routines
 */


/*********************/
/* Global variables */
/*******************/


// It's expected to exist the loading_dialog variable declared in the template HTML 
// This dialog should have two methods show() and hide() with obvious meaning.
//
// How it behaves it's up to the theme creator.


/*********************/
/* The kowview Object */
/*********************/

var kowview = new Object();



/*Error handling */
kowview.showErrorMessage = function( title, msg ) {
	if( kowview.isSet( kowview.messages[title] ) )
		console.log( kowview.messages[title] );
	else
		console.log( title );
	console.log( msg );
};


/* Loading */
kowview.showLoading = function( ) {
	loading_dialog.show();
};

kowview.hideLoading = function() {
	loading_dialog.hide();
};


/* helper methods */
kowview.isSet = function() {
	// FROM http://phpjs.org/functions/isset:454
	var a = arguments, l = a.length, i = 0, undef;
	
	if (l === 0) {
		throw new Error('Empty isSet'); 
	}
	
	while (i !== l) {
		if (a[i] === undef || a[i] === null) {
			return false; 
		}
		i++; 
	}
	return true;
};

kowview.isObject = function( obj ) {
	return ( typeof obj == 'object' )
};


/* json */
kowview.isSilent = function( parameters ) {
	if( kowview.isSet( parameters ) && kowview.isSet( parameters.silent ) )
			return parameters.silent;
	else
		return false;
};

kowview.buildParams = function( parameters, url ) {
	param = new Object();
	
	param.form = parameters.form;
	param.url = url;
	param.handleAs = 'json';
	
	param.load = function( responseObject, ioArgs ) {
			if( !kowview.isSilent( parameters ) ) {
				kowview.hideLoading();
			}
			
			parameters.load( responseObject );
		};
	param.error = function( data ) {
			if( !kowview.isSilent( parameters ) )
				kowview.hideLoading();
				
			console.dir( data );
			if( kowview.isSet( data.responseText ) && data.responseText != "" ) {
				response = dojo.fromJson( data.responseText );
				kowview.showErrorMessage( response.error, response.message );
				parameters.error( response );
			} else {
				kowview.showErrorMessage( "js.connection_failed", "Failed to connect to the server..." );
				parameters.error({
						error	: "js.connection_failed",
						message	: "Failed to connect to the server...",
						status	: "error"
					});
			}
		};
		
	return param;
};

kowview.postJson = function ( parameters, url ) {
		if( !kowview.isSilent( parameters ) )
			kowview.showLoading();
		dojo.xhrPost( kowview.buildParams( parameters, url ) );
};

kowview.getJson = function ( parameters, url ) {
		if( !kowview.isSilent( parameters ) )
			kowview.showLoading();
		dojo.xhrGet( kowview.buildParams( parameters, url ) );
};
