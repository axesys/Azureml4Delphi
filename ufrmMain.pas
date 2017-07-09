unit ufrmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses Webservice, Webservice.Input, Webservice.Interfaces, Webservice.Output;

procedure TForm1.Button1Click(Sender: TObject);
var
  Webservice: TWebservice;
  Input: TInput;
  Response: IWebserviceResponse;
  Output: TOutput;
  i: integer;
begin
  Webservice:= TWebservice.Create;
  Webservice.SetBaseURI('https://ussouthcentral.services.azureml.net/workspaces/ca13309b68c4495a852a5c4121283ed4/services/4fcb9cbf6d4b411f8c4c533686fef1cc/execute?api-version=2.0&details=true');
  Webservice.SetToken('ouxr2iG71HYkVyCowWQ20M4BaI0aVyRDDnRPntny4TPRrzZQagsmwEYjntcdeLz46LWn0qYQrffUaSR5Zfmiwg==');
  Input:= TInput.Create;
  with input.Inputs.Input1 do
  begin
    ColumnNames:= ['Sensor', 'Textura', 'Periodo'];
    Values:= [
      ['CS615','R', '183'],
      ['SP200' , 'MR', '640'],
      ['CSHS-II' , 'R', '224']
    ]
  end;
  Response := Webservice.Post(['.json'], Input.Json);
  Output:= TOutput.FromJsonString(Response.ContentAsString);
  for i:= 0 to Pred(output.Results.Output1.Value.Values.Count) do
    ShowMessage(Output.Results.Output1.Value.Values[i]);
end;

end.
