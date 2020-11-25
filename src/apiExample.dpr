program apiExample;

uses
  Vcl.Forms,
  uFrmMain in 'uFrmMain.pas' {Form1},
  uTelegramAPI.Interfaces in 'Common\uTelegramAPI.Interfaces.pas',
  uTelegramAPI in 'Common\uTelegramAPI.pas',
  uConsts in 'Common\uConsts.pas',
  uClassMessageDTO in 'Common\uClassMessageDTO.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
