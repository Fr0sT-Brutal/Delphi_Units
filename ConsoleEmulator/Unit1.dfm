object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 380
  ClientWidth = 235
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 147
    Height = 16
    Caption = #1055#1077#1088#1077#1084#1077#1085#1085#1099#1077' '#1086#1082#1088#1091#1078#1077#1085#1080#1103':'
  end
  object Label2: TLabel
    Left = 8
    Top = 127
    Width = 50
    Height = 16
    Caption = #1050#1086#1084#1072#1085#1076#1072
  end
  object Label3: TLabel
    Left = 8
    Top = 188
    Width = 56
    Height = 16
    Caption = #1051#1086#1075' '#1092#1072#1081#1083
  end
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 217
    Height = 89
    Lines.Strings = (
      'MyEnvVar=blabla'
      'foo=bar')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 8
    Top = 152
    Width = 217
    Height = 24
    TabOrder = 1
    Text = 'cmd /K set'
  end
  object Button1: TButton
    Left = 152
    Top = 248
    Width = 75
    Height = 25
    Caption = #1047#1072#1087#1091#1089#1090#1080#1090#1100
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 10
    Top = 210
    Width = 217
    Height = 24
    TabOrder = 3
    Text = 'console.log'
  end
  object Button2: TButton
    Left = 88
    Top = 327
    Width = 139
    Height = 34
    Caption = #1047#1072#1087#1091#1089#1082' '#1095#1077#1088#1077#1079' '#1087#1086#1090#1086#1082#1080
    TabOrder = 4
    OnClick = Button2Click
  end
end
