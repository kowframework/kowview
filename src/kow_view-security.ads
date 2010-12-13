------------------------------------------------------------------------------
--                                                                          --
--                          KOW Framework :: View                           --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
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
with Ada.Calendar;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;		use Ada.Strings.Unbounded;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Sec;
with KOW_Sec.Accounting;
with KOW_View.Components;		use KOW_View.Components;

---------
-- AWS --
---------

with AWS.Response;
with AWS.Session;
with AWS.Status;
with Templates_Parser;



package KOW_View.Security is
pragma Elaborate_Body( KOW_View.Security );

	---------------
	-- Variables --
	---------------
	
	Accountant : aliased KOW_Sec.Accounting.Accountant_Type := KOW_Sec.Accounting.New_Accountant( "security", KOW_View.Accountant'Access );


	----------------
	-- Components --
	----------------

	type Component_Type is new KOW_View.Components.Component_Type with private;
	


	package User_Data is new AWS.Session.Generic_Data(
			Data		=> KOW_Sec.User_Type,
			Null_Data	=> KOW_Sec.Logged_Anonymous_User
		);

	User_Key: constant String;


	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out KOW_Config.Config_File
		);
	-- Initialize the component while starting up the server
	-- Config is an already initialized configuration file located at:
	-- 	kowview/component_name
	--
	-- Configuration Parameters:
	-- 	login_error_page	:: default "/theme/login"
	--	Access_Denied_page	:: default "/theme/403"
	-- 	default_redirect	:: default "/"


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in KOW_Config.Config_File
		) return Module_Type'Class;
	-- no matter what module we request, the Criteria_Module_Module will be always called


	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		) return Service_Type'Class;


	-------------
	-- Modules --
	-------------

	--
	-- Criteria
	--

	type Criteria_Module is new Module_Type with private;
	-- a module is something that can be accessed anywhere inside the system.




	overriding
	procedure Initialize_Request(
			Module		: in out Criteria_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out AWS.Response.Data;
			Is_Final	: out    Boolean
		);
	-- if the user can access the page, do nothing.
	-- if it can't, then build a 'Location: access_denyed_page

	overriding
	procedure Process_Header(
			Module		: in out Criteria_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- used only for debugging
	-- print the expression criteria used as a HTML comment



	--
	-- Login Form
	--

	type Login_Form_Module is new Module_Type with private;

	overriding
	procedure Process_Request(
			Module		: in out Login_Form_Module;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		);
	-- put a login form into the response




	--------------
	-- Services --
	--------------


	type Login_Service is new Service_Type with private;
	-- try to login the user and redirect to the correct status page
	
	overriding
	procedure Process_Request(
			Service		: in out Login_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);



	type Logout_Service is new Service_Type with private;
	-- process the logout, if required, and redirect to some standard page

	overriding
	procedure Process_Request(
			Service		: in out Logout_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);


	type Switch_User_Service is new Service_Type with private;

	overriding
	procedure Process_Request(
			Service		: in out Switch_User_Service;
			Request		: in     AWS.Status.Data;
			Response	: in out AWS.Response.Data
		);
	

	-----------------------------
	-- User Session Management --
	-----------------------------


	function Is_Logged_In( Request : in AWS.Status.Data ) return Boolean;
	-- check if the user is logged in into the system

	function Get_User( Request : in AWS.Status.Data ) return KOW_Sec.User_Type;
	-- get the user object (or null) :)


	-----------------------------------
	-- Session Authorization Profile --
	-----------------------------------


	-- TODO :: implement the following for the sake of the security of this system ::

	-- What is it about:
	-- 
	-- It's a way for the application to grant some sort of authorization dinamically for the user session for later user.
	--
	-- What it means is, imagine a form for an entity.
	--
	-- There is a main service usually mapped to /entities. This is the place where forms are submited to store
	-- entity data. With no security check or whatsoever anyone could change anything in the database by knowing the entity
	-- structure (including user password).
	--
	-- To solve this, the page that renders the form can create an authorization key for this single session. This
	-- authorization key will be valid only for this session and the given entity/id (or without ID, when creating entities).
	--
	-- Moreover this authorization would be valid only for a short period of time (that can be changed by the application
	-- programmer).
	--
	-- Even more, it's gonna be a generic and precise way for handling authorizations throughout the application components.
	--
	

	type Authorization_Level_Type is ( Read, Create, Edit );
	-- this is what's used to map the authorizatrion level.
	-- it could mean anything depending on the contest... in the entity management the analogy is obvious.
	--
	-- Whapenns in here is that if anyone grant CREATE the user can request both READ and CREATE.

	procedure Grant_Authorization(
				Request			: in     AWS.Status.Data;
				Authorization_Key 	: in     String;
				Authorization_Level	: in     Authorization_Level_Type;
				Life_Time		: in     Duration := 300.0;
				Count			: in     Natural := 0
			);
	-- grant an authorization with the given key and level for the given life time
	-- when count > 0, allow only this given number of attempts
	

	procedure Request_Authorization(
				Request			: in     AWS.Status.Data;
				Authorization_Key	: in     String;
				Authorization_Level	: in     Authorization_Level_Type
			);
	-- tries to perform some change under some given authorization that should be granted before this call

private


	User_Key : constant String := "kow_sec.user";

	type Component_Type is new KOW_View.Components.Component_Type with record
		Default_Redirect	: Unbounded_String	:= To_Unbounded_String( "/" );
		Access_Denied_Page	: Unbounded_String	:= To_Unbounded_String( "/theme/403" );
		Login_Error_Page	: Unbounded_String	:= To_Unbounded_String( "/pages/login_error" );
	end record;


	type Criteria_Module is new Module_Type with record
		Expression		: Unbounded_String;
		Access_Denied_Page	: Unbounded_String;
	end record;


	type Login_Form_Module is new Module_Type with record
		Username_Label		: Unbounded_String;
		Password_Label		: Unbounded_String;
		Login_Label		: Unbounded_String;
		Redirect		: Unbounded_String;
		Template_Path		: Unbounded_String;

		Logged_In_As_Label	: Unbounded_String;
		Logout_Label		: Unbounded_String;
	end record;


	type Login_Service is new Service_Type with record
		Default_Redirect	: Unbounded_String;
		Login_Error_Page	: Unbounded_String;
	end record;

	type Logout_Service is new Service_Type with record
		Default_Redirect	: Unbounded_String;
	end record;

	type Switch_User_Service is new Service_Type with record
		Default_Redirect	: Unbounded_String;
		Criteria		: Unbounded_String;
	end record;


	---------------------------------------------------
	-- Private Part of Session Authorization Profile --
	---------------------------------------------------


	Authorization_Map_Key : constant String := "KOW_sec.authorization_map";

	
	type Authorization_Descriptor_Type is record
		Expiration_Time	: Ada.Calendar.Time;
		Level		: Authorization_Level_Type;
		Count		: Natural;
		-- TODO :: put COUNT really in use...
		-- for now it's just half implemented..
	end record;


	package Authorization_Maps is new Ada.Containers.Ordered_Maps(
				Key_Type	=> Unbounded_String,
				Element_Type	=> Authorization_Descriptor_Type
			);

	package Authorization_Data is new AWS.Session.Generic_Data(
			Data		=> Authorization_Maps.Map,
			Null_Data	=> Authorization_Maps.Empty_Map
		);



	protected Authorization_Manager is
		-- this is where the actual authorization granting/requesting happens.
		-- it's done this way to allow AJAX requests.
		procedure Grant_Authorization(
					Request			: in     AWS.Status.Data;
					Authorization_Key 	: in     String;
					Authorization_Level	: in     Authorization_Level_Type;
					Life_Time		: in     Duration := 300.0;
					Count			: in     Natural := 0
				);
		-- grant an authorization with the given key and level for the given life time
	

		procedure Request_Authorization(
					Request			: in     AWS.Status.Data;
					Authorization_Key	: in     String;
					Authorization_Level	: in     Authorization_Level_Type
				);
		-- tries to perform some change under some given authorization that should be granted before this call


	private
		My_Map : Authorization_Maps.Map;
	end Authorization_Manager;

end KOW_View.Security;
