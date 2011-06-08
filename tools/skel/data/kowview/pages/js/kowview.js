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



kowview.getTitle = function( title ) {
	if( kowview.isSet( kowview.messages[title] ) )
		return kowview.messages[title];
	else
		return title;
	
}

/*Messages */
kowview.showMessage = function( title, msg ) {
	theTitle = kowview.getTitle( title );

	dojo.byId( "messageDialogMessage" ).innerHTML = msg;
	messageDialog.set( 'title', theTitle );
	messageDialog.show();

};

kowview.hideMessage = function() {
	messageDialog.hide();
}

/*Error handling */
kowview.showErrorMessage = function( title, msg ) {
	theTitle = kowview.getTitle( title );

	dojo.byId("errorDialogMessage").innerHTML = msg;
	errorDialog.set( 'title', theTitle );
	errorDialog.show();
};

kowview.hideErrorMessage = function () {
	errorDialog.hide();
}

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

	param.url = url;
	param.handleAs = 'json';
	param.method = 'post';


	function setParam( key ) {
		if( kowview.isSet( parameters[key] ) )
			param[key] = parameters[key];
	}


	/* Get and post variables */
	dojo.forEach( ["form", "sync", "preventCache", "content", "headers", "timeout", "user", "password", "handle", "failOk"], setParam);

	/* Post only variables */
	setParam( "postData" );



	
	param.load = function( responseObject, ioArgs ) {
			if( !kowview.isSilent( parameters ) ) {
				kowview.hideLoading();
			}

			if( !kowview.isSet( responseObject.status ) ) {
				console.log("erroooo");
				this.error( responseObject, ioArgs );
			} else if( responseObject.status == "redirect" ) {
				window.location.href = responseObject.to;
			} else if( kowview.isSet( responseObject.error ) ) {
				return this.error( responseObject, ioArgs );
			} else if( kowview.isSet( parameters.load ) ) {
				parameters.load( responseObject );
			}
			return responseObject;
		};
	param.error = function( data, ioArgs ) {
			console.log( "found the following error:" );
			console.dir( data );
			if( !kowview.isSilent( parameters ) )
				kowview.hideLoading();
			
			try{
				if( kowview.isSet( data.error )  || ( kowview.isSet( data.responseText ) && data.responseText != "" ) ) {
					if( kowview.isSet( data.error ) )
						response = data;
					else
						response = dojo.fromJson( data.responseText );

					if( response.error =="KOW_VIEW.ENTITIES.VALIDATION.VALIDATION_ERROR" ) {
						splited = response.message.split("|");
						columnName = splited[1];
						errorMessage = splited[2];
	
						column = dijit.byId( param.form[columnName].id );
						if( kowview.isSet( column ) ) {
							if( kowview.isSet( column.displayMessage ) ) {
								column.displayMessage( errorMessage );
							} else {
								kowview.showErrorMessage( response.error, errorMessage );
							}
						} else {
							kowview.showErrorMessage( response.error, errorMessage );
						}
	
					} else {
						kowview.showErrorMessage( response.error, response.message );
					}
					//kowview.showErrorMessage( response.error, response.message );
					if( kowview.isSet( parameters.error ) ) {
						try{
							parameters.error( response );
						} catch( e ) {
							console.log( "error in the user function" );
							console.dir( response );
							console.dir( e );
						}
					} else {
						kowview.showErrorMessage( response.error, response.message );
					}
				} else {
					if( kowview.isSet( parameters.error ) ) {
						parameters.error({
								error	: "js.connection_failed",
								message	: "Failed to connect to the server...",
								status	: "error"
							});
					} else {
						kowview.showErrorMessage( "js.connection_failed", "Failed to connect to the server..." );
					}
				}
			} catch( e ) {
				console.log("error while processing error status" );
				console.dir( data );
				console.dir( e );
				kowview.showErrorMessage( "js.unknown_error", "Seems like you found a javascript bug in this application. Please contact the closest tecnology responsible." );
			}
			return data;
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


kowview.iframeSend = function ( parameters, url ) {
		if( url.indexOf( "?" ) > -1 )
			url += "&";
		else
			url += "?";
		url += "iframe=true";

		if( !kowview.isSilent( parameters ) )
			kowview.showLoading();
		dojo.io.iframe.send( kowview.buildParams( parameters, url ) );
};
