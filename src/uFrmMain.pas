unit uFrmMain;

interface

uses
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, uTelegramAPI, uTelegramAPI.Interfaces, uConsts,
  Vcl.StdCtrls, uClassMessageDTO, Vcl.ExtCtrls, Vcl.Buttons, System.IOUtils;

type
  TForm1 = class(TForm)
    BtnSendFile: TButton;
    EdtTokenBot: TEdit;
    Memo1: TMemo;
    BtnSendMsg: TButton;
    BtnSendWithButtons: TButton;
    Button1: TButton;
    BtnSendLocation: TButton;
    Button2: TButton;
    SpeedButton1: TSpeedButton;
    EdtUserId: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnSendFileClick(Sender: TObject);
    procedure BtnSendMsgClick(Sender: TObject);
    procedure BtnSendWithButtonsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure BtnSendLocationClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

  Memo1.Clear;
end;

procedure TForm1.BtnSendFileClick(Sender: TObject);
begin
  FTelegram.SendFile('C:\File.zip');

  Memo1.Text := FTelegram.GetResult();
end;

procedure TForm1.BtnSendMsgClick(Sender: TObject);
begin
  FTelegram.SendMsg('Hey there!');

  Memo1.Text := FTelegram.GetResult();
end;

procedure TForm1.BtnSendWithButtonsClick(Sender: TObject);
var
  pButtons: TTelegramButtons;
begin
  pButtons := TTelegramButtons.Create;
  with pButtons do
  begin
    Add('Lamp 1 Off', 'https://domain.com/lamp1/off');
    Add('Lamp 1 On', 'https://domain.com/lamp1/on');
    Add('Lamp 2 Off', 'https://domain.com/lamp2/off');
    Add('Lamp 2 On', 'https://domain.com/lamp2/on');
  end;

  FTelegram.SendMsgWithButtons('Hi!', pButtons);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  pChatList: TChatMessageDTOList;
  pChat: TChatMessageDTO;
begin
  pChatList := TChatMessageDTOList.Create;
  pChat := TChatMessageDTO.Create;

  FTelegram.GetUpdates(pChatList);

  for pChat in pChatList do
    ShowMessage(pChat.Message.Text + ' - ' + pChat.Message.From.Id.ToString);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not Assigned(FAsyncHttp) then
  begin
    FAsyncHttp := TThread.CreateAnonymousThread(
      procedure
      var
        pChatList: TChatMessageDTOList;
      begin
        pChatList := TChatMessageDTOList.Create;

        while True do
        begin
          pChatList.Clear;

          FTelegram.GetUpdates(pChatList);

          FAsyncHttp.Synchronize(FAsyncHttp,
            procedure
            var
              pChat: TChatMessageDTO;
            begin
              Memo1.Lines.Clear;
              Memo1.Lines.Add('-' + TimeToStr(Now()));

              for pChat in pChatList do
                Memo1.Lines.Add(pChat.Message.Text);

              Memo1.Lines.Add('-');
            end);

          FAsyncHttp.Sleep(5000);
        end;

      end);

    FAsyncHttp.Start();
  end;
end;

procedure TForm1.BtnSendLocationClick(Sender: TObject);
begin
  FTelegram.SendLocation('-23.3765579', '-51.9195364,16z');
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
