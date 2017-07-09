unit Webservice.Input;

interface

uses Classes, Generics.Collections, System.Json;

type

TGlobalParameters = class
private
public
  function Json: TJSONValue;
end;

TInput1 = class
private
  FColumnNames: TArray<string>;
  FValues: TArray<TArray<string>>;
public
  property ColumnNames: TArray<string> read FColumnNames write FColumnNames;
  property Values: TArray<TArray<String>> read FValues write FValues;
  function Json: TJSONValue;
end;

TInputs = class
private
  FInput1: TInput1;
public
  property input1: TInput1 read FInput1 write FInput1;
  constructor Create;
  destructor Destroy; override;
  function Json: TJSONValue;
end;

TInput = class
private
  FGlobalParameters: TGlobalParameters;
  FInputs: TInputs;
public
  property GlobalParameters: TGlobalParameters read FGlobalParameters write FGlobalParameters;
  property Inputs: TInputs read FInputs write FInputs;
  constructor Create;
  destructor Destroy; override;
  function Json: TJSONValue;
end;

implementation

{TGlobalParameters}


function TGlobalParameters.Json: TJSONValue;
begin
  result := TJSONObject.Create;
end;

{TInput1}

function TInput1.Json: TJSONValue;
var
  jvInput1: TJSONObject;
  jvColumnNames: TJSONArray;
  jvValues: TJSONArray;
  jvValue: TJSONArray;
  i,j: Integer;
begin
  jvInput1:= TJSONObject.Create;
  jvColumnNames:= TJSONArray.Create;
  jvValues:= TJSONArray.Create;
  for i:= 0 to Pred(Length(FColumnNames)) do jvColumnNames.Add(FColumnNames[i]);
  for i:= 0 to Pred(Length(FValues)) do
  begin
    jvValue:= TJSONArray.Create;
    for j:= 0 to Pred(Length(FValues[i])) do
      jvValue.Add(FValues[i][j]);
    jvValues.Add(jvValue);
  end;
  jvInput1.AddPair('ColumnNames', jvColumnNames);
  jvInput1.AddPair('Values', jvValues);
  result := jvInput1;
end;

{TInputs}

constructor TInputs.Create;
begin
  inherited;
  FInput1 := TInput1.Create();
end;

destructor TInputs.Destroy;
begin
  FInput1.free;
  inherited;
end;

function TInputs.Json: TJSONValue;
var
  jvInputs: TJSONObject;
begin
  jvInputs:= TJSONObject.Create;
  jvInputs.AddPair('input1', FInput1.JSON);
  result := jvInputs;
end;

{TRootClass}

constructor TInput.Create;
begin
  inherited;
  FInputs := TInputs.Create();
  FGlobalParameters := TGlobalParameters.Create();
end;

destructor TInput.Destroy;
begin
  FInputs.free;
  FGlobalParameters.free;
  inherited;
end;

function TInput.Json: TJSONValue;
var
  Root: TJSONObject;
begin
  Root:= TJSONObject.Create;
  Root.AddPair('Inputs', FInputs.Json);
  Root.AddPair('GlobalParameters', FGlobalParameters.Json);
  result := Root;
end;

end.
