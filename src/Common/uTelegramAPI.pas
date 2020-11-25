unit uTelegramAPI;

interface

uses
  uTelegramAPI.Interfaces, uConsts, System.Net.HttpClientComponent,
  System.SysUtils, System.Classes, System.Net.Mime, System.JSON,
  uClassMessageDTO, Rest.JSON, Rest.JSON.Types;

// UnixToDateTime(1605808194) DateUtils

type
  TTelegramAPI = Class(TInterfacedObject, iTelegramAPI)
  private
    FHTTPClient: TNetHTTPClient;
    FBotToken: String;
    FUserID: String;
    FResult: String;
    FProcErrorException: TProcErrorException;
    function _POST(AUrl: String; AData: TStrings): String; overload;
    function _POST(AUrl: String; AData: TMultipartFormData): String; overload;
    function _GET(AUrl: String): String;
    function GetURL(APath: String = ''): String;
    function Ready(): Boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    class function New(): iTelegramAPI;
    function SetBotToken(AToken: String): iTelegramAPI;
    function SetUserID(AUserID: String): iTelegramAPI;
    function SendFile(AFileName: String): iTelegramAPI;
    function SendMsg(AMsg: String): iTelegramAPI;
    function SendMsgWithButtons(AMsg: String; AButtons: TTelegramButtons)
      : iTelegramAPI;
    function OnError(AValue: TProcErrorException): iTelegramAPI;
    function GetResult: String;
    function GetUpdates(var AValue: TChatMessageDTOList): iTelegramAPI;
    function SendLocation(ALatitude, ALongitude: String): iTelegramAPI;
  end;

const
  urlBase = 'https://api.telegram.org/{BOT_TOKEN}';

implementation

{ TTelegramAPI }

constructor TTelegramAPI.Create;
begin
  FHTTPClient := TNetHTTPClient.Create(nil);
  FHTTPClient.ConnectionTimeout := 5000;
  FHTTPClient.ResponseTimeout := 5000;
end;

destructor TTelegramAPI.Destroy;
begin
  FreeAndNil(FHTTPClient);
  inherited;
end;

class function TTelegramAPI.New: iTelegramAPI;
begin
  Result := Self.Create;
end;

function TTelegramAPI.OnError(AValue: TProcErrorException): iTelegramAPI;
begin
  Result := Self;
  FProcErrorException := AValue;
end;

function TTelegramAPI.Ready: Boolean;
begin
  Result := True;

  if FBotToken.IsEmpty then
  begin
    if Assigned(FProcErrorException) then
      FProcErrorException(Exception.Create('BotToken is Empty!'));
    Result := False;
  end
  else if FUserID.IsEmpty then
  begin
    if Assigned(FProcErrorException) then
      FProcErrorException(Exception.Create('UserID is Empty!'));
    Result := False;
  end;
end;

function TTelegramAPI.GetResult: String;
begin
  Result := FResult;
end;

function TTelegramAPI.GetUpdates(var AValue: TChatMessageDTOList): iTelegramAPI;
var
  pArrJSON: TJSONArray;
  I: Byte;
begin
  Result := Self;

  if FBotToken.IsEmpty then
  begin
    if Assigned(FProcErrorException) then
      FProcErrorException(Exception.Create('BotToken is Empty!'));
    Exit;
  end;

  try
    FResult := _GET(GetURL('/getUpdates'));

    pArrJSON := ((TJSONObject.ParseJSONValue(FResult) as TJSONObject)
      .GetValue('result') as TJSONArray);

    if pArrJSON.Count <= 0 then Exit;

    for I := 0 to Pred(pArrJSON.Count) do
      AValue.Add(TJSON.JsonToObject<TChatMessageDTO>(pArrJSON.Items[I].ToJSON));
  except
    on E: Exception do
    begin
      if Assigned(FProcErrorException) then
        FProcErrorException(E);
    end;
  end;
end;

function TTelegramAPI.GetURL(APath: String = ''): String;
begin
  Result := EmptyStr;

  try
    Result := urlBase.Replace('{BOT_TOKEN}', FBotToken + APath);
  except
  end;
end;

function TTelegramAPI.SendFile(AFileName: String): iTelegramAPI;
var
  pData: TMultipartFormData;
begin
  Result := Self;
  FHTTPClient.ContentType := 'multipart/form-data';

  pData := TMultipartFormData.Create;
  pData.AddFile('document', AFileName);
  pData.AddField('chat_id', FUserID);

  FResult := _POST(GetURL('/sendDocument'), pData);
end;

function TTelegramAPI.SendLocation(ALatitude, ALongitude: String): iTelegramAPI;
var
  pData: TStrings;
begin
  Result := Self;

  FHTTPClient.ContentType := 'application/json';

  pData := TStringList.Create;
  pData.AddPair('chat_id', FUserID);
  pData.AddPair('latitude', ALatitude);
  pData.AddPair('longitude', ALongitude);

  FResult := _POST(GetURL('/sendLocation'), pData);
end;

function TTelegramAPI.SendMsg(AMsg: String): iTelegramAPI;
var
  pData: TStrings;
begin
  Result := Self;

  FHTTPClient.ContentType := 'application/json';

  pData := TStringList.Create;
  pData.AddPair('chat_id', FUserID);
  pData.AddPair('text', AMsg);

  FResult := _POST(GetURL('/sendMessage'), pData);
end;

function TTelegramAPI.SendMsgWithButtons(AMsg: String;
  AButtons: TTelegramButtons): iTelegramAPI;
var
  pData: TStrings;
  pJsonArr: TJSONArray;
begin
  Result := Self;

  if AButtons.Count <= 0 then
    Exit;

  pJsonArr := TJSONArray.Create;

  for var Enum in AButtons do
  begin
    pJsonArr.AddElement(TJSONObject.Create.AddPair('text', Enum.Key)
      .AddPair('url', Enum.Value));
  end;

  FHTTPClient.ContentType := 'application/json';

  pData := TStringList.Create;
  pData.AddPair('chat_id', FUserID);
  pData.AddPair('text', AMsg);
  pData.AddPair('reply_markup', '{"inline_keyboard":[' +
    pJsonArr.ToJSON + ']}');

  FResult := _POST(GetURL('/sendMessage'), pData);
end;

function TTelegramAPI.SetBotToken(AToken: String): iTelegramAPI;
begin
  Result := Self;
  FBotToken := 'bot' + AToken;
end;

function TTelegramAPI.SetUserID(AUserID: String): iTelegramAPI;
begin
  Result := Self;
  FUserID := AUserID;
end;

function TTelegramAPI._GET(AUrl: String): String;
begin
  Result := EmptyStr;

  try
    Result := FHTTPClient.Get(AUrl).ContentAsString(TEncoding.UTF8);
  except
    on E: Exception do
    begin
      if Assigned(FProcErrorException) then
        FProcErrorException(E);
    end;
  end;
end;

function TTelegramAPI._POST(AUrl: String; AData: TMultipartFormData): String;
begin
  Result := EmptyStr;

  if not Ready() then
    Exit;

  try
    Result := FHTTPClient.Post(AUrl, AData).ContentAsString(TEncoding.UTF8);
  except
    on E: Exception do
    begin
      if Assigned(FProcErrorException) then
        FProcErrorException(E);
    end;
  end;
end;

function TTelegramAPI._POST(AUrl: String; AData: TStrings): String;
begin
  Result := EmptyStr;

  if not Ready() then
    Exit;

  try
    Result := FHTTPClient.Post(AUrl, AData).ContentAsString(TEncoding.UTF8);
  except
    on E: Exception do
    begin
      if Assigned(FProcErrorException) then
        FProcErrorException(E);
    end;
  end;
end;

end.
