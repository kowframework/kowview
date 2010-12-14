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
			User: KOW_Sec.User_Type := KOW_Sec.Do_Login( Username, Password );


			Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
		begin
			User_Data.Set( Session_ID, User_Key, User );
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



	procedure Process_Request(
			Service		: in out Switch_User_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		) is
		use KOW_Sec;
		use KOW_Sec.Authorization_Criterias;

		P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

		User_Identity : String := AWS.Parameters.Get( P, "user_identity" );


		Session_ID  : AWS.Session.ID := AWS.Status.Session (Request);
		The_User : KOW_Sec.User_Type := User_Data.Get( Session_ID, User_Key );


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

		declare
			use KOW_Sec;
			use KOW_Sec.Authorization_Criterias;
			Role_Criteria	: Criteria_Interface'Class := Create_Role_Criteria( To_Criteria_Descriptor( "kow_view.security::su" ) );
			Extra_Criteria	: Criteria_Interface'Class := Create_Expression_Criteria( Criteria_Descriptor( Service.Criteria ) );
		begin
			if Is_Anonymous( The_User ) then
				raise ACCESS_DENIED with "you need to be logged in to do this";
			end if;
			

			KOW_Sec.Accounting.Require( Role_Criteria, The_User, Accountant'Access );
			KOW_Sec.Accounting.Require( Extra_Criteria, The_User, Accountant'Access );
		end;


		-- if we got here then we can go on


		if User_Identity = "" then
			raise CONSTRAINT_ERROR with "User Identity required for switch user service";
		end if;


		-- TODO :: see if it is possible to reset the session
		-- AWS.Session.Delete( Session_ID );
		-- simply clear the current session and continue..

		
		declare
			User: KOW_Sec.User_Type := (
							Data		=> KOW_Sec.Get_User( User_Identity ),
							Current_manager	=> The_user.Current_Manager
						);
		begin
			Session_ID := AWS.Status.Session (Request);
			-- new session now.. :D
			User_Data.Set( Session_ID, User_Key, User );
		end;



		
		Response := AWS.Response.URL( Redirect );

	end Process_Request;




