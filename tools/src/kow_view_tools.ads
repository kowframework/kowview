


--------------
-- Ada 2005 --
--------------
with Ada.Directories;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

package KOW_View_Tools is

	Skel_Path	: constant Unbounded_String := To_Unbounded_String( 
								Ada.Directories.Full_Name( "/etc/kvdriver/skel" )
								);





	function Project_Name return String;
	function Destination_Path( Name : in String ) return String;
	function Destination_Path( Name : in Unbounded_String ) return String;

end KOW_View_Tools;
