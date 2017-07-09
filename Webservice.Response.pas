unit Webservice.Response;

interface

uses
  Webservice.Interfaces,
  System.SysUtils,
  System.Net.HttpClient;

type

  TWebserviceResponse = class(TInterfacedObject, IWebserviceResponse)
  private
    FHttpResponse: IHTTPResponse;
  public
    constructor Create(AHTTPResponse: IHTTPResponse);
    function ContentAsString(const AEncoding: TEncoding = nil): string;
  end;

implementation

{ TWebserviceResponse }

function TWebserviceResponse.ContentAsString(const AEncoding
  : TEncoding = nil): string;
begin
  Result := FHttpResponse.ContentAsString(AEncoding);
end;

constructor TWebserviceResponse.Create(AHTTPResponse: IHTTPResponse);
begin
  inherited Create;
  FHttpResponse := AHTTPResponse;
end;

end.
