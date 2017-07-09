program Webservices;

uses
  System.StartUpCopy,
  FMX.Forms,
  ufrmMain in 'ufrmMain.pas' {Form1},
  Webservice.Interfaces in 'Webservice.Interfaces.pas',
  Webservice.Request in 'Webservice.Request.pas',
  Webservice.Response in 'Webservice.Response.pas',
  Webservice in 'Webservice.pas',
  Webservice.Output in 'Webservice.Output.pas',
  Webservice.Input in 'Webservice.Input.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
