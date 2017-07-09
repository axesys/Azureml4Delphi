unit Webservice.Request;

interface

uses
  Webservice.Interfaces,
  Webservice.Response,
  System.JSON,
  System.SysUtils,
  System.Net.URLClient,
  System.Classes,
  System.Net.HttpClient,
  System.Generics.Collections;

type

  TWebserviceRequest = class(TInterfacedObject, IWebserviceRequest)
  protected
    FBaseURI: string;
    FToken: string;
    function EncodeResourceParams(AResourceParams: array of string): string;
    function EncodeQueryParams(AQueryParams
      : TDictionary<string, string>): string;
    function EncodeToken(const AToken: string): string;
  public
    constructor Create(const ABaseURI: string = ''; const AToken: string = '');
    procedure SetBaseURI(const ABaseURI: string);
    procedure SetToken(const AToken: string);
    function SendData(const AResourceParams: array of string;
      const ACommand: TWebserviceCommand; AData: TJSONValue = nil;
      AQueryParams: TDictionary < string, string >= nil;
      ADataOwner: boolean = true): IWebserviceResponse;
    property BaseURI: string read FBaseURI write SetBaseURI;
    property Token: string read FToken write SetToken;
  end;

implementation

uses
  System.NetConsts, System.NetEncoding, System.StrUtils;

{ TWebserviceRequest }

procedure TWebserviceRequest.SetBaseURI(const ABaseURI: string);
begin
  FBaseURI := ABaseURI;
end;

procedure TWebserviceRequest.SetToken(const AToken: string);
begin
  FToken := AToken;
end;

function TWebserviceRequest.SendData(const AResourceParams: array of string;
  const ACommand: TWebserviceCommand; AData: TJSONValue = nil;
  AQueryParams: TDictionary<string, string> = nil; ADataOwner: boolean = true)
  : IWebserviceResponse;
var
  LClient: THTTPClient;
  LResp: IHTTPResponse;
  LURL: string;
  LSource: TStringStream;
begin
  try
    LClient := THTTPClient.Create;
    LClient.ContentType := 'application/json';
    LClient.CustomHeaders['Authorization']:= EncodeToken(FToken);
    try
      LSource := nil;
      if AData <> nil then
        LSource := TStringStream.Create(AData.ToJSON);
      try
        LURL := BaseURI + EncodeResourceParams(AResourceParams) +
          EncodeQueryParams(AQueryParams);
        case ACommand of
          fcPut:
            LResp := LClient.Put(LURL, LSource);
          fcPost:
            LResp := LClient.Post(LURL, LSource);
          fcPatch:
            LResp := LClient.Patch(LURL, LSource);
          fcGet:
            LResp := LClient.Get(LURL);
          fcRemove:
            LResp := LClient.Delete(LURL);
        end;
        Result := TWebserviceResponse.Create(LResp);
      finally
        if Assigned(LSource) then
          LSource.Free;
      end;
    finally
      LClient.Free;
    end;
  finally
    if ADataOwner then
    begin
      if Assigned(AData) then
        AData.Free;
    end;
  end;
end;

constructor TWebserviceRequest.Create(const ABaseURI, AToken: string);
begin
  inherited Create;
  FBaseURI := ABaseURI;
  FToken := AToken;
end;

function TWebserviceRequest.EncodeQueryParams(AQueryParams
  : TDictionary<string, string>): string;
var
  Param: TPair<string, string>;
begin
  if (not Assigned(AQueryParams)) or not(AQueryParams.Count > 0) then
    exit('');
  Result := ifthen(Token.IsEmpty, '?', '');
  for Param in AQueryParams do
  begin
    if Result <> '?' then
      Result := Result + '&';
    Result := Result + TNetEncoding.URL.URLDecode(Param.Key) + '=' +
      TNetEncoding.URL.URLDecode(Param.Value)
  end;
end;

function TWebserviceRequest.EncodeResourceParams(AResourceParams
  : array of string): string;
var
  i: integer;
begin
  Result := '';
  for i := low(AResourceParams) to high(AResourceParams) do
    Result := Result + '/' + TNetEncoding.URL.Encode(AResourceParams[i]);
end;

function TWebserviceRequest.EncodeToken(const AToken: string): string;
begin
  if Token.IsEmpty then
    Result := ''
  else
    Result := 'Bearer ' + Token;
end;

end.
