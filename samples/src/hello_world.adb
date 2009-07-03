




--------------
-- Ada 2005 --
--------------
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

---------------
-- Ada Works --
---------------
with Aw_Config;
with Aw_View.Components;	use Aw_View.Components;

---------
-- AWS --
---------
with AWS.Status;
with Templates_Parser;

package body Hello_World is


	overriding
	procedure Initialize(
			Component	: in out Component_Type;
			Component_Name	: in     String;
			Config		: in out Aw_Config.Config_File
		) is
	begin
		Component.Name := To_Unbounded_String( Component_Name );
		Component.User := Aw_Config.Value( Config, "user", "Anonymous" );
	end Initialize;

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		)
		return Module_Instance_Interface'Class is

	begin
		if Module_Name = "echo" then
			declare
				Module: Echo;
			begin
				Module.Message := Aw_Config.Element( Config, "message" );
				return Module;
			end;
		else
			raise MODULE_ERROR with Module_Name;
		end if;
	end Create_Instance;

	overriding
	function Create_Instance(
			Component	: in Component_Type;
			Service_Name	: in String;
			Service_Mapping	: in String
		)
		return Service_Instance_Interface'Class is
		v: void;
	begin
		raise SERVICE_ERROR with "no service available at all";

		return v;
	end Create_Instance;

	-- the echo module:
	


	overriding
	procedure Process_Request(
			Module		: in out Echo;
			Request		: in     AWS.Status.Data;
			Parameters	: in out Templates_Parser.Translate_Set;
			Response	: in out Unbounded_String
		) is
	begin
		Response := Module.Message;
	end Process_Request;
end Hello_World;

