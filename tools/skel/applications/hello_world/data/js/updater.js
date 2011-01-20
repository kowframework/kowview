




function POST_Update_Hello_World_Counter( module_id ) {
	container = dojo.byId( "counter_" + module_id );
	console.log( "using POST method" );
	kowview.modules.postJson(
			module_id,
			{
				form	: dojo.byId( "form_" + module_id ),
				load	: function( data ) {
						console.dir( data );
						container.innerHTML = data.response.counter;
					},
				error	: function( data ) {
						container.innerHTML = "<span class='highlight'>I can't talk to the server! oops</span>";
					},
				silent	: false
			}
		);

}


function GET_Update_Hello_World_Counter( module_id ) {
	container = dojo.byId( "counter_" + module_id );
	console.log( "using GET method" );
	kowview.modules.getJson(
			module_id,
			{
				form	: dojo.byId( "form_" + module_id ),
				load	: function( data ) {
						console.dir( data );
						container.innerHTML = data.response.counter;
					},
				error	: function( data ) {
						container.innerHTML = "<span class='highlight'>I can't talk to the server! oops</span>";
					},
				silent	: false
			}
		);

}
