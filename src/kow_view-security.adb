

---------
-- Ada --
---------
with Ada.Calendar;			use Ada.Calendar;
with Ada.Directories;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------

with KOW_Config;
with KOW_Lib.File_System;
with KOW_Sec;
with KOW_Sec.Authorization_Criterias;
with KOW_View.Components_Registry;


---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;
with AWS.Session;
with AWS.Status;
with Templates_Parser;



package body KOW_View.Security is

	----------------
	-- Components --
	----------------
	


	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		) is
		-- Initializie the component while starting up the server
		-- Config is an already initialized configuration file located at:
		-- 	kowview/component_name
		--
		-- Configuration Parameters:
		-- 	login_error_page	:: default "/theme/login"
		--	Access_Denied_page	:: default "/theme/403"
		-- 	default_redirect	:: default "/"
	begin
		Component.Login_Error_Page := KOW_Config.Value(
						F	=> Config,
						Key	=> "login_error_page",
						Default	=> Component.Login_Error_Page
					);

		Component.Access_Denied_Page := KOW_Config.Value(
						F	=> Config,
						Key	=> "access_denied_page",
						Default	=> Component.Access_Denied_Page
					);

		Component.Default_Redirect := KOW_Config.Value(
						F	=> Config,
						Key	=> "default_redirect",
						Default	=> Component.Default_Redirect
					);
	end Initialize;


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return Module_Instance_Interface'Class is
		-- no matter what module we request, the Criteria_Module_Module will be always called

		function Get_Resolved_Path( Key, Default : in String ) return Unbounded_String is
			Name : String := KOW_Config.Value( Config, Key, Default );
		begin
			return To_Unbounded_String(
					KOW_View.Components_Registry.Locate_Resource(
							Component_Name  => "security",
							Resource        => Name,
							Extension       => "html",
							Kind            => Ada.Directories.Ordinary_File
						)
				);
		end Get_Resolved_Path;

	begin

		if Module_Name = "criteria" then
			declare
				Module: Criteria_Module;
			begin
				
				Module.Expression := KOW_Config.Element(
								f	=> Config,
								Key	=> "expression"
							);


				Module.Access_Denied_Page := Component.Access_Denied_Page;
				return Module;
			end;
		elsif Module_Name = "login_form" then
			declare
				use KOW_Lib.File_System;
				Module			: Login_Form_Module;
			begin

				Module.Username_Label	:= KOW_Config.Value( Config, "username_label", "Username" );
				Module.Password_Label	:= KOW_Config.Value( Config, "password_label", "Password" );
				Module.Login_Label	:= KOW_Config.Value( Config, "login_label", "ok" );
				Module.Redirect		:= KOW_Config.Value( Config, "redirect", "" );
				Module.Template_Path	:= Get_Resolved_Path(
										Key	=> "template",
										Default	=> "default_templates" & Separator & "login_form"
									);
				Module.Logged_in_as_Label	:= KOW_Config.Value( Config, "logged_in_as_label", "Welcome" );
				Module.Logout_Label		:= KOW_Config.Value( Config, "logout_label", "logout" );
				return Module;
			end;
		else
			raise KOW_View.Components.MODULE_ERROR with "no module called """ & Module_Name & """ in ""pages"" component.";
		end if;

	end Create_Instance;

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Instance_Interface'Class is
	begin
		if Service_Name = "login" then
			declare
				Service: Login_Service;
			begin
				Service.Default_Redirect := Component.Default_Redirect;
				Service.Login_Error_Page   := Component.Login_Error_Page;

				return Service;
			end;
		elsif Service_Name = "logout" then
			declare
				Service: Logout_Service;
			begin
				Service.Default_Redirect := Component.Default_Redirect;

				return Service;
			end;
		else
			raise SERVICE_ERROR with "Service """ & Service_Name & """ doesn't exist";
		end if;
	end Create_Instance;


	-------------
	-- Modules --
	-------------


	--
	-- Criteria
	--


	overriding
	procedure Initialize_Request(
			Module		: in out Criteria_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		) is
		-- if the user can access the page, do nothing.
		-- if it can't, then build a 'Location: access_denyed_page
		use KOW_Sec;
		use KOW_Sec.Authorization_Criterias;
		Criteria_Object: Criteria'Class := Create_Expressions_Criteria(
					Criteria_Descriptor(
						Module.Expression
					)
				);
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
		User_Object : KOW_Sec.User_Access := User_Data.Get( Session_ID, User_Key );


	begin
		if User_Object = NULL then
			raise ACCESS_DENIED;
		end if;

		Require( User_Object.all, Criteria_Object );
		-- notice: CONSTRAINT_ERROR shouldn't be raised as we've checked if
		-- the user objectis null before calling this procedure!

		Is_Final := False;
	exception
		when KOW_Sec.ACCESS_DENIED =>
			Is_Final := TRUE;
			Response := AWS.Response.URL( To_String ( Module.Access_Denied_Page ) );
	end Initialize_Request;

	procedure Process_Header(
			Module		: in out Criteria_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		-- used only for debugging
		-- print the expression criteria used as a HTML comment

		Begin_Comment	: Unbounded_String := To_Unbounded_String( "<!-- " );
		End_Comment	: Unbounded_String := To_Unbounded_String( " // --> " );
	begin
		Response := Response & Begin_Comment & Module.Expression & End_Comment;
	end Process_Header;






	--
	-- Login Form 
	--


	overriding
	procedure Process_Request(
			Module		: in out Login_Form_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
		use KOW_Sec;

		My_Parameters	: Templates_Parser.Translate_Set := Parameters;
		User_Object	: KOW_Sec.User_Access := Get_User( Request );

	begin

		Templates_Parser.Insert(
				My_Parameters,
				Templates_Parser.Assoc(
					"username_label",
					Module.Username_Label
				)
			);
		Templates_Parser.Insert(
				My_Parameters,
				Templates_Parser.Assoc(
					"password_label",
					Module.Password_Label
				)
			);
		Templates_Parser.Insert(
				My_Parameters,
				Templates_Parser.Assoc(
					"login_label",
					Module.Login_Label
				)
			);
		Templates_Parser.Insert(
			My_Parameters,
			Templates_Parser.Assoc(
					"redirect",
					Module.Redirect
				)
			);

		if User_Object = null then
			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc(
						"is_logged_in",
						False
					)
				);
		else
			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc(
						"is_logged_in",
						True
					)
				);
			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc(
						"user_identity",
						KOW_Sec.Identity( User_Object.All )
					)
				);

			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc(
						"user_full_name",
						KOW_Sec.Full_Name( User_Object.All )
					)
				);


			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc(
						"logged_in_as_label",
						Module.Logged_in_as_Label
					)
				);

			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc(
						"user_gravatar_url",
						Gravatar_URL( User_Object.all )
					)
				);

			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc(
						"logout_label",
						Module.Logout_Label
					)
				);

		end if;

		Response := Response &
			To_Unbounded_String(
				Templates_Parser.Parse(
						To_String( Module.Template_Path ),
						My_Parameters
					)
				);


	end Process_Request;





	--------------
	-- Services --
	--------------


	procedure Process_Request(
			Service		: in out Login_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is


		P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

		Username	: String := AWS.Parameters.Get( P, "username" );
		Password	: String := AWS.Parameters.Get( P, "password" );

		function Redirect return String is
			Pragma Inline( Redirect );

			R: String := AWS.Parameters.Get( P, "redirect" );
		begin
			if R = "" then
				return To_String( Service.Default_Redirect );
			end if;

			return R;
		end Redirect;
			


	begin
		if Username = "" OR Password = "" then
			raise CONSTRAINT_ERROR with "Username or Password required for Login service";
		end if;
		
		declare
			User_Object: KOW_Sec.User'Class := KOW_Sec.Do_Login( Username, Password );


			Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
		begin
			User_Data.Set( Session_ID, User_Key, KOW_Sec.To_Access( User_Object ) );
		end;

		Response := AWS.Response.URL( Redirect );
		-- the user is logged in... time to continue

	exception
		when OTHERS =>
			-- when anything wrong happens... redirect to the error page
			Response := AWS.Response.URL( To_String( Service.Login_Error_PAge ) );
	end Process_Request;




	procedure Process_Request(
			Service		: in out Logout_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is

		P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);

		function Redirect return String is
			Pragma Inline( Redirect );

			R: String := AWS.Parameters.Get( P, "redirect" );
		begin
			if R = "" then
				return To_String( Service.Default_Redirect );
			end if;

			return R;
		end Redirect;
	begin
		AWS.Session.Delete( Session_ID );
		-- simply clear the current session and continue..

		Response := AWS.Response.URL( Redirect );

	end Process_Request;



	-----------------------------
	-- User Session Management --
	-----------------------------


	function Is_Logged_In( Request : in AWS.Status.Data ) return Boolean is
		-- check if the user is logged in into the system
		use KOW_Sec;
	begin
		return Get_User( Request ) /= Null;
	end Is_Logged_In;

	function Get_User( Request : in AWS.Status.Data ) return KOW_Sec.User_Access is
		-- get the user object (or null) :)
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
	begin
		return User_Data.Get( Session_ID, User_Key );
	end Get_User;



	-----------------------------------
	-- Session Authorization Profile --
	-----------------------------------



	procedure Grant_Authorization(
				Request			: in     AWS.Status.Data;
				Authorization_Key 	: in     String;
				Authorization_Level	: in     Authorization_Level_Type;
				Life_Time		: in     Duration := 300.0;
				Count			: in     Natural := 0
			) is
		-- grant an authorization with the given key and level for the given life time
	begin
		Authorization_Manager.Grant_Authorization(
				Request,
				Authorization_Key,
				Authorization_Level,
				Life_Time,
				Count
			);
	end Grant_Authorization;


	procedure Request_Authorization(
				Request			: in     AWS.Status.Data;
				Authorization_Key	: in     String;
				Authorization_Level	: in     Authorization_Level_Type
			) is
		-- tries to perform some change under some given authorization that should be granted before this call
	begin
		Authorization_Manager.Request_Authorization(
				Request,
				Authorization_Key,
				Authorization_Level
			);
	end Request_Authorization;




	---------------------------------------------------
	-- Private Part of Session Authorization Profile --
	---------------------------------------------------



	protected body Authorization_Manager is
		-- this is where the actual authorization granting/requesting happens.
		-- it's done this way to allow AJAX requests.
		procedure Grant_Authorization(
					Request			: in     AWS.Status.Data;
					Authorization_Key 	: in     String;
					Authorization_Level	: in     Authorization_Level_Type;
					Life_Time		: in     Duration := 300.0;
					Count			: in     Natural := 0
				) is
			-- grant an authorization with the given key and level for the given life time


			My_Key	: Unbounded_String := To_Unbounded_String( Authorization_Key );
			Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
			The_Map	: Authorization_Maps.Map := Authorization_Data.Get( Session_ID, Authorization_Map_Key );
			-- if the THe_Map is null, no worries... it's only an empty map and it's time to fill it in with some
			-- more data

			My_Descriptor : Authorization_Descriptor_Type;

			My_Expiration_Time : Ada.Calendar.Time := Clock + Life_Time;

			use Authorization_Maps;
		begin
			My_Descriptor := Element( The_Map, My_Key );


			-- if got here, there is an element with the same key...
			-- time to update it
			
			if My_Descriptor.Expiration_Time < My_Expiration_Time then
				My_Descriptor.Expiration_Time := My_Expiration_Time;
			end if;

			if My_Descriptor.Level < Authorization_Level then
				My_Descriptor.Level := Authorization_Level;
			end if;

			if My_Descriptor.Count < Count or ( Count = 0 AND My_Descriptor.Count /= 0 ) then
				My_Descriptor.Count := Count;
			end if;


			Include( The_Map, My_Key, My_Descriptor );
			Authorization_Data.Set( Session_ID, Authorization_Map_Key, The_Map );

		exception
			when Constraint_Error =>
				-- there is no element My_Key in the map..
				My_Descriptor := ( Expiration_Time => Clock + Life_Time, Level => Authorization_Level, Count => Count );
				Insert( The_Map, My_key, My_Descriptor );
				Authorization_Data.Set( Session_ID, Authorization_Map_Key, The_Map );
		end Grant_Authorization;

		procedure Request_Authorization(
					Request			: in     AWS.Status.Data;
					Authorization_Key	: in     String;
					Authorization_Level	: in     Authorization_Level_Type
				) is
			-- tries to perform some change under some given authorization that should be granted before this call
			
			My_Key : Unbounded_String := To_Unbounded_String( Authorization_Key );
			Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
			The_Map	: Authorization_Maps.Map := Authorization_Data.Get( Session_ID, Authorization_Map_Key );
			-- if the THe_Map is null, no worries... it's only an empty map and it's time to fill it in with some
			-- more data

			My_Descriptor : Authorization_Descriptor_Type;
			
			The_Clock : Ada.Calendar.Time := Clock;


			use Authorization_Maps;

		begin

			My_Descriptor := Element( The_Map, My_key );
			-- if got here, there is was any level at any time...
			--
			-- lets try it out;


			if My_Descriptor.Expiration_Time < The_Clock then
				raise KOW_Sec.ACCESS_DENIED with "your authorization key for """ & Authorization_Key & """ has been expired.";
			elsif My_Descriptor.Level < Authorization_Level then
				raise KOW_Sec.ACCESS_DENIED with
						"you do not have enought permission to access """ &
						Authorization_Key & """ at """ &
						Authorization_Level_Type'Image( Authorization_Level ) & """ level";
			end if;
		exception
			when CONSTRAINT_ERROR =>
				raise KOW_Sec.ACCESS_DENIED with "you don't have permission to access """ & Authorization_Key & """";
		end Request_Authorization;
	end Authorization_Manager;



end KOW_View.Security;
