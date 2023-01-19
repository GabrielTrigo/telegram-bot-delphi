object Form1: TForm1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  ClientHeight = 412
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Consolas'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 178
    Top = 56
    Width = 87
    Height = 21
    Caption = 'Save'
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 5
    Top = 43
    Width = 48
    Height = 13
    Caption = 'User Id:'
  end
  object Label2: TLabel
    Left = 5
    Top = 5
    Width = 60
    Height = 13
    Caption = 'Bot Token:'
  end
  object BtnSendFile: TButton
    Left = 106
    Top = 91
    Width = 95
    Height = 25
    Caption = 'SendFile'
    TabOrder = 0
    OnClick = BtnSendFileClick
  end
  object EdtTokenBot: TEdit
    Left = 5
    Top = 18
    Width = 260
    Height = 21
    Color = clSilver
    TabOrder = 1
  end
  object MemLog: TMemo
    Left = 0
    Top = 158
    Width = 411
    Height = 254
    Align = alBottom
    BorderStyle = bsNone
    Color = 14277119
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    ExplicitLeft = 5
    ExplicitTop = 153
  end
  object BtnSendMsg: TButton
    Left = 5
    Top = 91
    Width = 95
    Height = 25
    Caption = 'SendMsg'
    TabOrder = 3
    OnClick = BtnSendMsgClick
  end
  object BtnSendWithButtons: TButton
    Left = 5
    Top = 122
    Width = 196
    Height = 25
    Caption = 'SendMsgWithButtons'
    TabOrder = 4
    OnClick = BtnSendWithButtonsClick
  end
  object BtnGetUpdates: TButton
    Left = 207
    Top = 91
    Width = 95
    Height = 25
    Caption = 'GetUpdates'
    TabOrder = 5
    OnClick = BtnGetUpdatesClick
  end
  object BtnSendLocation: TButton
    Left = 308
    Top = 91
    Width = 95
    Height = 25
    Caption = 'SendLocation'
    TabOrder = 6
    OnClick = BtnSendLocationClick
  end
  object BtnStartMonitor: TButton
    Left = 207
    Top = 122
    Width = 196
    Height = 25
    Caption = 'Start monitor'
    TabOrder = 7
    OnClick = BtnStartMonitorClick
  end
  object EdtUserId: TEdit
    Left = 5
    Top = 56
    Width = 167
    Height = 21
    Color = clSilver
    TabOrder = 8
  end
end
