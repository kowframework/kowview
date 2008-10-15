
---------------
-- Ada Works --
---------------

with Aw_Config;
with Aw_View.Components;		use Aw_View.Components;



package Aw_View.Component_Registry is
	type ac is new Aw_View.Components.Component_Interface with null record;

	overriding
	function Create_Instance(
			Component	: in ac;
			Service		: in String;
			Config		: in Aw_Config.Config_File
		) return Service_Instance_Interface'Class;

	overriding
	function Create_Instance(
			Component: in ac;
			Module: in String;
			Config: in Aw_Config.Config_File 
		) return Module_Instance_Interface'Class;



	type as is new Service_Instance_Interface with null record;

	type am is new Module_Instance_Interface with null record;





	function Load( Component_Name: in String ) return Aw_View.Components.Component_Interface'Class;
	-- Loads a component by it's name
	

	function Load_Service( Component_Name, Service_Name: in String ) return Service_Instance_Interface'Class;
	-- load a service by it's component name and it's name

	function Load_Module(
			Component_Name	: in String;
			Module_Name	: in String;
			Config		: in Aw_Config.Config_File
		) return Module_Instance_Interface'Class;
	-- get a module instance

end Aw_View.Component_Registry;
