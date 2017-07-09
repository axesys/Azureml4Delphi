unit Webservice;

interface

uses
  Webservice.Interfaces,
  Webservice.Response,
  Webservice.Request,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.Classes,
  System.JSON,
  System.SysUtils,
  System.Generics.Collections;

type

  TWebservice = class(TInterfacedObject, IWebservice)
  private
    FBaseURI: string;
    FToken: string;
  public
    procedure SetBaseURI(const ABaseURI: string);
    procedure SetToken(const AToken: string);
    function Get(const AParams: array of string;
      AQueryParams: TDictionary<string, string> = nil): IWebserviceResponse;
    function Put(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil;
      ADataOwner: boolean = true): IWebserviceResponse;
    function Post(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil;
      ADataOwner: boolean = true): IWebserviceResponse;
    function Patch(const AParams: array of string; AData: TJSONValue = nil;
      AQueryParams: TDictionary<string, string> = nil;
      ADataOwner: boolean = true): IWebserviceResponse;
    function Delete(const AParams: array of string;
      AQueryParams: TDictionary<string, string> = nil): IWebserviceResponse;
    property BaseURI: string read FBaseURI write SetBaseURI;
    property Token: string read FToken write SetToken;
  end;

implementation

{ TWebservice }

procedure TWebservice.SetBaseURI(const ABaseURI: string);
begin
  FBaseURI := ABaseURI;
end;

procedure TWebservice.SetToken(const AToken: string);
begin
  FToken := AToken;
end;

function TWebservice.Get(const AParams: array of string;
  AQueryParams: TDictionary<string, string> = nil): IWebserviceResponse;
var
  ARequest: IWebserviceRequest;
begin
  ARequest := TWebserviceRequest.Create(BaseURI, Token);
  Result := ARequest.SendData(AParams, TWebserviceCommand.fcGet, nil,
    AQueryParams);
end;

function TWebservice.Post(const AParams: array of string;
  AData: TJSONValue = nil; AQueryParams: TDictionary<string, string> = nil;
  ADataOwner: boolean = true): IWebserviceResponse;
var
  ARequest: IWebserviceRequest;
begin
  ARequest := TWebserviceRequest.Create(BaseURI, Token);
  Result := ARequest.SendData(AParams, TWebserviceCommand.fcPost, AData,
    AQueryParams, ADataOwner);
end;

function TWebservice.Put(const AParams: array of string;
  AData: TJSONValue = nil; AQueryParams: TDictionary<string, string> = nil;
  ADataOwner: boolean = true): IWebserviceResponse;
var
  ARequest: IWebserviceRequest;
begin
  ARequest := TWebserviceRequest.Create(BaseURI, Token);
  Result := ARequest.SendData(AParams, TWebserviceCommand.fcPut, AData,
    AQueryParams, ADataOwner);
end;

function TWebservice.Patch(const AParams: array of string;
  AData: TJSONValue = nil; AQueryParams: TDictionary<string, string> = nil;
  ADataOwner: boolean = true): IWebserviceResponse;
var
  ARequest: IWebserviceRequest;
begin
  ARequest := TWebserviceRequest.Create(BaseURI, Token);
  Result := ARequest.SendData(AParams, TWebserviceCommand.fcPatch, AData,
    AQueryParams, ADataOwner);
end;

function TWebservice.Delete(const AParams: array of string;
  AQueryParams: TDictionary<string, string> = nil): IWebserviceResponse;
var
  ARequest: IWebserviceRequest;
begin
  ARequest := TWebserviceRequest.Create(BaseURI, Token);
  Result := ARequest.SendData(AParams, TWebserviceCommand.fcRemove, nil,
    AQueryParams);
end;

end.
