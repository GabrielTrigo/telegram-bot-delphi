unit uFrmMain;

interface

uses
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, System.IOUtils,
  uTelegramAPI, uTelegramAPI.Interfaces, uConsts, uClassMessageDTO;

type
  TForm1 = class(TForm)
    BtnSendFile: TButton;
    EdtTokenBot: TEdit;
    MemLog: TMemo;
    BtnSendMsg: TButton;
    BtnSendWithButtons: TButton;
    BtnGetUpdates: TButton;
    BtnSendLocation: TButton;
    BtnStartMonitor: TButton;
    SpeedButton1: TSpeedButton;
    EdtUserId: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnSendFileClick(Sender: TObject);
    procedure BtnSendMsgClick(Sender: TObject);
    procedure BtnSendWithButtonsClick(Sender: TObject);
    procedure BtnGetUpdatesClick(Sender: TObject);
    procedure BtnSendLocationClick(Sender: TObject);
    procedure BtnStartMonitorClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    FTelegram: iTelegramAPI;
    FAsyncHttp: TThread;
    procedure OnAPIError(AExcept: Exception);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  EdtTokenBot.Text := 'YOUR_API_TOKEN_HERE';

  // use the button "getUpdates" for get chats IDs(only messages sent to your bot)
  EdtUserId.Text := 'USERID_FOR_SEND_MESSAGES';

  FTelegram := TTelegramAPI.New();
  FTelegram
    .OnError(OnAPIError)
    .SetUserID(EdtUserId.Text)
    .SetBotToken(EdtTokenBot.Text);

  MemLog.Clear;
end;

procedure TForm1.BtnSendFileClick(Sender: TObject);
begin
  FTelegram.SendFile('C:\File.zip');

  MemLog.Text := FTelegram.GetResult();
end;

procedure TForm1.BtnSendMsgClick(Sender: TObject);
begin
  FTelegram.SendMsg('Hey there!');

  MemLog.Text := FTelegram.GetResult();
end;

procedure TForm1.BtnSendWithButtonsClick(Sender: TObject);
var
  lButtons: TTelegramButtons;
begin
  lButtons := TTelegramButtons.Create;

  try
    with lButtons do
    begin
      Add('Lamp 1 Off', 'https://domain.com/lamp1/off');
      Add('Lamp 1 On', 'https://domain.com/lamp1/on');
      Add('Lamp 2 Off', 'https://domain.com/lamp2/off');
      Add('Lamp 2 On', 'https://domain.com/lamp2/on');
    end;

    FTelegram.SendMsgWithButtons('Hi!', lButtons);
  finally
    FreeAndNil(lButtons);
  end;
end;

procedure TForm1.BtnGetUpdatesClick(Sender: TObject);
var
  pChatList: TChatMessageDTOList;
  pChat: TChatMessageDTO;
begin
  pChatList := TChatMessageDTOList.Create;
  pChat := TChatMessageDTO.Create;

  try
    FTelegram.GetUpdates(pChatList);

    for pChat in pChatList do
      ShowMessage(pChat.Message.Text + ' - ' + pChat.Message.From.Id.ToString);
  finally
    FreeAndNil(pChat);
    FreeAndNil(pChatList);
  end;
end;

procedure TForm1.BtnStartMonitorClick(Sender: TObject);
begin
  if Assigned(FAsyncHttp) then Exit;

  FAsyncHttp := TThread.CreateAnonymousThread(
    procedure
    var
      lChatList: TChatMessageDTOList;
    begin
      lChatList := TChatMessageDTOList.Create;

      while True do
      begin
        lChatList.Clear;

        FTelegram.GetUpdates(lChatList);

        FAsyncHttp.Synchronize(FAsyncHttp,
          procedure
          var
            lChat: TChatMessageDTO;
          begin
            MemLog.Lines.Clear;
            MemLog.Lines.Add('-' + TimeToStr(Now()));

            for lChat in lChatList do
              MemLog.Lines.Add(lChat.Message.Text);

            MemLog.Lines.Add('-');
          end);

        FAsyncHttp.Sleep(5000);
      end;

    end);

  FAsyncHttp.Start();
end;

procedure TForm1.BtnSendLocationClick(Sender: TObject);
begin
  FTelegram.SendLocation('51.519138', '-0.129028');
end;

procedure TForm1.OnAPIError(AExcept: Exception);
begin
  TThread.Synchronize(TThread.Current, procedure
  begin
    MessageDlg(AExcept.Message, mtWarning, [mbOK], 0);
  end);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  FTelegram.SetBotToken(EdtTokenBot.Text);
  FTelegram.SetUserID(EdtUserId.Text);
end;

end.
