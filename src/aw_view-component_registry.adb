
---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_View.Components;		use Aw_View.Components;



package body Aw_View.Component_Registry is



	function Create_Instance(
			Component	: in ac;
			Service		: in String;
			Config		: in Aw_Config.Config_File
		) return Service_Instance_Interface'Class is
		a: as;
	begin
		return a;
	end Create_Instance;


	function Create_Instance(
			Component: in ac;
			Module: in String;
			Config: in Aw_Config.Config_File 
		) return Module_Instance_Interface'Class is
		a: am;
	begin
		return a;
	end Create_Instance;



	function Load( Component_Name: in String ) return Aw_View.Components.Component_Interface'Class is
		-- Loads a component by it's name
		a: ac;
	begin
		return a;
	end Load;

	function Load_Service( Component_Name, Service_Name: in String ) return Service_Instance_Interface'Class is
		-- load a service by it's component name and it's name
		a: as;
	begin
		return a;
	end Load_Service;

	function Load_Module(
			Component_Name	: in String;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class is
		-- get a module instance
		a: am;
	begin
		return a;
	end Load_Module;

end Aw_View.Component_Registry;
