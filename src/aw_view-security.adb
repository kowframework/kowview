

---------
-- Ada --
---------
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_Sec;
with Aw_Sec.Authorization_Criterias;

---------
-- AWS --
---------
with AWS.Parameters;
with AWS.Response;
with AWS.Session;
with AWS.Status;
with Templates_Parser;



package body Aw_View.Security is

	----------------
	-- Components --
	----------------
	


	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		) is
		-- Initializie the component while starting up the server
		-- Config is an already initialized configuration file located at:
		-- 	awview/component_name
		--
		-- Configuration Parameters:
		-- 	login_error_page	:: default "/theme/login"
		--	Access_Denied_page	:: default "/theme/403"
		-- 	default_redirect	:: default "/"
	begin
		Component.Login_Error_Page := Aw_Config.Value(
						F	=> Config,
						Key	=> "login_error_page",
						Default	=> Component.Login_Error_Page
					);

		Component.Access_Denied_Page := Aw_Config.Value(
						F	=> Config,
						Key	=> "access_denied_page",
						Default	=> Component.Access_Denied_Page
					);

		Component.Default_Redirect := Aw_Config.Value(
						F	=> Config,
						Key	=> "default_redirect",
						Default	=> Component.Default_Redirect
					);
	end Initialize;


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class is
		-- no matter what module we request, the Criteria_Module_Module will be always called
		Module: Criteria_Module;
	begin

		Module.Expression := Aw_Config.Element(
						f	=> Config,
						Key	=> "expression"
					);
		Module.Access_Denied_Page := Component.Access_Denied_Page;
		return Module;
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
		use Aw_Sec;
		use Aw_Sec.Authorization_Criterias;
		Criteria_Object: Criteria'Class := Create_Expressions_Criteria(
					Criteria_Descriptor(
						Module.Expression
					)
				);
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
		User_Object : Aw_Sec.User_Access := User_Data.Get( Session_ID, User_Key );


	begin
		if User_Object = NULL then
			raise ACCESS_DENIED;
		end if;

		Require( User_Object.all, Criteria_Object );
		-- notice: CONSTRAINT_ERROR shouldn't be raised as we've checked if
		-- the user objectis null before calling this procedure!

	exception
		when Aw_Sec.ACCESS_DENIED =>
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
			User_Object: Aw_Sec.User'Class := Aw_Sec.Do_Login( Username, Password );


			Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
		begin
			User_Data.Set( Session_ID, User_Key, Aw_Sec.To_Access( User_Object ) );
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

end Aw_View.Security;
