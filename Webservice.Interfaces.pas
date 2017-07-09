unit Webservice.Interfaces;

interface

uses
  System.JSON,
  System.SysUtils,
  System.Generics.Collections;

type

  TWebserviceCommand = (fcPut, fcPatch, fcPost, fcGet, fcRemove);

  IWebserviceResponse = interface(IInterface)
    ['{28CE1C37-DE9E-47C2-8764-2FB073B93FB8}']
    function ContentAsString(const AEncoding: TEncoding = nil): string;
  end;

  IWebserviceRequest = interface(IInterface)
    ['{3B265C49-747A-4EFF-AC76-138A39F1C34B}']
    procedure SetBaseURI(const ABaseURI: string);
    procedure SetToken(const AToken: string);
    function SendData(const AResourceParams: array of string;
      const ACommand: TWebserviceCommand; AData: TJSONValue = nil;
      AQueryParams: TDictionary < string, string >= nil;
      ADataOwner: boolean = true): IWebserviceResponse;
  end;

  IWebservice = interface(IInterface)
    ['{43135DC9-C04F-42A3-AB5B-3E15AE207322}']
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
  end;

implementation

end.
