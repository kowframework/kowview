

with Ada.Text_IO;		use Ada.Text_IO;


with AWS.SMTP;
with AWS.SMTP.Client;
procedure emailtest is

	Server		: AWS.SMTP.Receiver := AWS.SMTP.Initialize( "localhost" );

	Attachments	: AWS.SMTP.Client.Attachment_Set( 2 .. 1 );
	Status		: AWS.SMTP.Status;
begin
	AWS.SMTP.Client.Send(
			Server		=> Server,
			From		=> AWS.SMTP.E_Mail( "Marcelo", "marcelo@somehost" ),
			To		=> AWS.SMTP.E_Mail( "Marcelo", "marcelo@someotherhost" ),
			Subject		=> "teste",
			Message		=> "um grande teste de mensagem!",
			Attachments	=> Attachments,
			Status		=> Status
		);
	if AWS.SMTP.Is_OK( Status ) then
		Put_Line( "Email enviado!" );
	else
		Put_Line( AWS.SMTP.Status_Message( Status ) );
	end if;
end emailtest;
