


var hello_world_dialog;

dojo.addOnLoad(function() {
	hello_world_dialog = new dijit.Dialog( {
			title	: "Hello World",
			style	: "width : 300px"
		});
});

function Hello_World_Info() {
	hello_world_dialog.attr( "content", "You are runnig the Hello World example application for the KOW Framework. This is a script implemented in the information.js file.... look for it in the application data folder :)" );
	hello_world_dialog.show();
}
