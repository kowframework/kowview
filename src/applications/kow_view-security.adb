------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWView is free software; you can redistribute it  and/or modify it under--
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWView is distributed in the hope that it will be useful,but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWView; see file COPYING.  If not, write--
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------


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
with KOW_Sec.Accounting;
with KOW_Sec.Authorization_Criterias;
with KOW_View.Components.Registry;


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
		) return Module_Type'Class is
		-- no matter what module we request, the Criteria_Module_Module will be always called

		function Get_Resolved_Path( Key, Default : in String ) return Unbounded_String is
			Name : String := KOW_Config.Value( Config, Key, Default );
		begin
			return To_Unbounded_String(
					KOW_View.Components.Registry.Locate_Resource(
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
		) return Service_Type'Class is
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
		elsif Service_Name = "switch_user" then
			declare
				Service: Switch_User_Service;
			begin
				Service.Default_Redirect := Component.Default_Redirect;
				Service.Criteria := To_Unbounded_String( "GROUPS={admin}" ); -- TODO :: make it configurable

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
		Criteria_Object: Criteria_Interface'Class := Create_Expression_Criteria( Criteria_Descriptor( Module.Expression ) );
		Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
		User : KOW_Sec.User_Type := User_Data.Get( Session_ID, User_Key );


	begin
		KOW_Sec.Accounting.Require( Criteria_Object, User, Accountant'Access );
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
		User	: KOW_Sec.User_Type := Get_User( Request );

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

		if Is_Anonymous( User ) then
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
						String( User.Data.Identity )
					)
				);

			Templates_Parser.Insert(
					My_Parameters,
					Templates_Parser.Assoc(
						"user_full_name",
						KOW_Sec.Full_Name( User.Data )
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
						Gravatar_URL( User.Data )
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





	-----------------------------
	-- User Session Management --
	-----------------------------


	function Is_Logged_In( Request : in AWS.Status.Data ) return Boolean is
		-- check if the user is logged in into the system
		use KOW_Sec;
	begin
		return not Is_Anonymous( Get_User( Request ) );
	end Is_Logged_In;

	function Get_User( Request : in AWS.Status.Data ) return KOW_Sec.User_Type is
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


begin

	KOW_Sec.Roles_Registry.Register(
				Application	=> "kow_view.security",
				Role		=> "su"
			);

end KOW_View.Security;