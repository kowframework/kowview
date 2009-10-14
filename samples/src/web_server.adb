




---------------
-- Ada Works --
---------------
with APQ;
with APQ.MySQL.Client;
with KOW_Config;
with KOW_Ent;
with KOW_View.Components_Registry;	use KOW_View.Components_Registry;
with KOW_View.Components;		use KOW_View.Components;
with KOW_View.Entities;
with KOW_View.Pages;
with KOW_View.Security;
with KOW_View.Service_Mapping;
with KOW_View.Themes;



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
	Conn		: KOW_Ent.Connection_Ptr := new APQ.Mysql.Client.Connection_Type;
begin

	---------------------
	-- KOW_Config Setup --
	---------------------
	
	KOW_Config.Set_Project_Name( "WEB_SERVER" );


	--------------------------
	-- APQ and KOW_Ent Setup --
	--------------------------

	APQ.Set_Host_Name( Conn.all, "localhost" );
	APQ.Set_User( Conn.all, "root" );
	APQ.Set_Password( Conn.all, "senharoot" );
	APQ.Set_DB_Name( Conn.all, "awent_samples" );

	APQ.Connect( Conn.all );


	APQ.Open_DB_Trace( Conn.all, "apq.log" );

	KOW_Ent.Set_Connection( Conn );


	------------------
	-- KOW_Ent Setup --
	------------------
	KOW_Ent.Labels.Reload_Registry;

	-------------------
	-- KOW_View Setup --
	-------------------

	Register(
		"security",
		new KOW_View.Security.Component_Type,
		true
	);

	Register(
		"themes",
		new KOW_View.Themes.Component_Type,
		true
	);

	Register(
		"pages",
		new KOW_View.Pages.Component_Type,
		true
	);


	Register(
		"entities",
		new KOW_View.Entities.Component_Type,
		false
	);

	Register(
		"hello_world",
		new Hello_World.Component_Type,
		true
	);


	KOW_View.Service_Mapping.Reload_Mappings;


	AWS.Server.Start(
		Web_Server,
		KOW_View.Service_Mapping.AWS_Callback'Unrestricted_Access,
		Conf
	);
	

	AWS.Server.Wait( AWS.Server.Forever );



end web_server;
