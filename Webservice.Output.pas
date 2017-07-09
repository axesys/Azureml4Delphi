unit Webservice.Output;

interface

uses Classes, Generics.Collections, System.Json;

type

TValue = class
private
  FValues: TStrings;
public
  property Values: TStrings read FValues write FValues;
  constructor Create(AJsonString: string);
  destructor Destroy; override;
  class function FromJsonString(AJsonString: string): TValue;
end;

TOutput1 = class
private
  FType: String;
  FValue: TValue;
public
  property &type: String read FType write FType;
  property Value: TValue read FValue write FValue;
  constructor Create(AJsonString: string);
  destructor Destroy; override;
  class function FromJsonString(AJsonString: string): TOutput1;
end;

TResults = class
private
  FOutput1: TOutput1;
public
  property Output1: TOutput1 read FOutput1 write FOutput1;
  constructor Create(AJsonString: string);
  destructor Destroy; override;
  class function FromJsonString(AJsonString: string): TResults;
end;

TOutput = class
private
  FResults: TResults;
public
  property Results: TResults read FResults write FResults;
  constructor Create(AJsonString: string);
  destructor Destroy; override;
  class function FromJsonString(AJsonString: string): TOutput;
end;

implementation

{TValue}

constructor TValue.Create(AJsonString: string);
var
  jvResult: TJSONObject;
  jvResults: TJSONObject;
  jvOutput1: TJSONObject;
  jvValue: TJSONObject;
  jvValues: TJSONArray;
  i: Integer;
begin
  jvResult:= TJSONObject.ParseJSONValue(AJsonString) as TJSONObject;
  jvResults:= jvResult.GetValue('Results') as TJSONObject;
  jvOutput1:= jvResults.GetValue('output1') as TJSONObject;
  jvValue:= jvOutput1.GetValue('value') as TJSONObject;
  jvValues:= jvValue.GetValue('Values') as TJSONArray;
  FValues:= TStringList.Create;
  for i:= 0 to Pred(jvValues.Count) do
    FValues.Add((jvValues.Items[i] as TJSONArray).Items[0].Value);
end;

destructor TValue.Destroy;
begin
  FValues.Free;
  inherited;
end;

class function TValue.FromJsonString(AJsonString: string): TValue;
begin
  result := TValue.Create(AJsonString);
end;

{TOutput1}

constructor TOutput1.Create;
begin
  FValue := TValue.FromJsonString(AJsonString);
end;

destructor TOutput1.Destroy;
begin
  FValue.free;
  inherited;
end;

class function TOutput1.FromJsonString(AJsonString: string): TOutput1;
begin
  result := TOutput1.Create(AJsonString);
end;

{TResults}

constructor TResults.Create(AJsonString: string);
begin
  FOutput1 := TOutput1.FromJsonString(AJsonString);
end;

destructor TResults.Destroy;
begin
  FOutput1.free;
  inherited;
end;

class function TResults.FromJsonString(AJsonString: string): TResults;
begin
  result := TResults.Create(AJsonString);
end;

{TOutput}

constructor TOutput.Create(AJsonString: string);
begin
  FResults := TResults.FromJsonString(AJsonString);
end;

destructor TOutput.Destroy;
begin
  FResults.free;
  inherited;
end;

class function TOutput.FromJsonString(AJsonString: string): TOutput;
begin
  result := TOutput.Create(AJsonString);
end;

end.
