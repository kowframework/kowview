




---------------
-- Ada Works --
---------------
with APQ;
with APQ.MySQL.Client;
with Aw_Config;
with Aw_Ent;
with Aw_View.Components_Registry;	use Aw_View.Components_Registry;
with Aw_View.Components;		use Aw_View.Components;
with Aw_View.Entities;
with Aw_View.Pages;
with Aw_View.Security;
with Aw_View.Service_Mapping;
with Aw_View.Themes;



---------
-- AWS --
---------

with AWS.Config;
with AWS.Server;

--------------
-- Ada 2005 --
--------------

with Ada.Text_IO;		use Ada.Text_IO;


----------
-- Demo --
----------
with Book_Entities;
pragma Elaborate( Book_Entities );
with Hello_World;

procedure web_server is
	Web_Server	: AWS.Server.HTTP;
	Conf		: constant AWS.Config.Object := AWS.Config.Get_Current;
	Conn		: Aw_Ent.Connection_Ptr := new APQ.Mysql.Client.Connection_Type;
begin

	---------------------
	-- Aw_Config Setup --
	---------------------
	
	Aw_Config.Set_Project_Name( "WEB_SERVER" );


	--------------------------
	-- APQ and Aw_Ent Setup --
	--------------------------

	APQ.Set_Host_Name( Conn.all, "localhost" );
	APQ.Set_User( Conn.all, "root" );
	APQ.Set_Password( Conn.all, "senharoot" );
	APQ.Set_DB_Name( Conn.all, "awent_samples" );

	APQ.Connect( Conn.all );


	APQ.Open_DB_Trace( Conn.all, "apq.log" );

	Aw_Ent.Set_Connection( Conn );


	------------------
	-- Aw_Ent Setup --
	------------------
	Aw_Ent.Labels.Reload_Registry;

	-------------------
	-- Aw_View Setup --
	-------------------

	Register(
		"security",
		new Aw_View.Security.Component_Type,
		true
	);

	Register(
		"themes",
		new Aw_View.Themes.Component_Type,
		true
	);

	Register(
		"pages",
		new Aw_View.Pages.Component_Type,
		true
	);


	Register(
		"entities",
		new Aw_View.Entities.Component_Type,
		false
	);

	Register(
		"hello_world",
		new Hello_World.Component_Type,
		true
	);


	Aw_View.Service_Mapping.Reload_Mappings;


	AWS.Server.Start(
		Web_Server,
		Aw_View.Service_Mapping.AWS_Callback'Unrestricted_Access,
		Conf
	);
	

	AWS.Server.Wait( AWS.Server.Forever );



end web_server;
