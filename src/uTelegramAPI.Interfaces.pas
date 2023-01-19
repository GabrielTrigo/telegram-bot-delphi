unit uTelegramAPI.Interfaces;

interface

uses uConsts, System.SysUtils, uClassMessageDTO;

type
  iTelegramAPI = interface
    ['{11C947AD-6E74-47BF-8056-1EAA96D3A925}']
    function SetBotToken(AToken: String): iTelegramAPI;
    function SetUserID(AUserID: String): iTelegramAPI;
    function SendFile(AFileName: String): iTelegramAPI;
    function SendMsg(AMsg: String): iTelegramAPI;
    function SendMsgWithButtons(AMsg: String; AButtons: TTelegramButtons)
      : iTelegramAPI;
    function OnError(AValue: TProcErrorException): iTelegramAPI;
    function GetUpdates(var AValue: TChatMessageDTOList): iTelegramAPI;
    function GetResult(): String;
    function SendLocation(ALatitude, ALongitude: String): iTelegramAPI;
  end;

implementation

end.
