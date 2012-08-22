



procedure Process_Custom_Request is
begin




	for Region in Regions loop
	end loop;

end Process_Custom_Request;




procedure Process_json_Request is
	Region	: Get_Region( Module_Id );
	Index	: Get_Index( Module_Id );
begin

	Process_json_Request(
				Module	=> Get_Module( Region )( Index )
			);
end process_json_request;
