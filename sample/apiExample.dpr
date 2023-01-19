program apiExample;

uses
  Vcl.Forms,
  uFrmMain in 'uFrmMain.pas' {Form1},
  uClassMessageDTO in '..\src\uClassMessageDTO.pas',
  uConsts in '..\src\uConsts.pas',
  uTelegramAPI.Interfaces in '..\src\uTelegramAPI.Interfaces.pas',
  uTelegramAPI in '..\src\uTelegramAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
