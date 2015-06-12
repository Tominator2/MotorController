object Form1: TForm1
  Left = 194
  Top = 153
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'USB Rumble Motor Controller'
  ClientHeight = 214
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ExitButton: TButton
    Left = 200
    Top = 176
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 0
    OnClick = ExitButtonClick
  end
  object SelectButton: TButton
    Left = 8
    Top = 176
    Width = 89
    Height = 25
    Caption = 'Choose Joystick'
    TabOrder = 1
    OnClick = SelectButtonClick
  end
  object LeftGroupBox: TGroupBox
    Left = 8
    Top = 16
    Width = 265
    Height = 65
    Caption = 'Left Motor'
    TabOrder = 2
    object LeftPowerLabel: TLabel
      Left = 243
      Top = 31
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object LeftPowerBar: TScrollBar
      Left = 64
      Top = 20
      Width = 153
      Height = 17
      LargeChange = 10
      Max = 254
      PageSize = 0
      TabOrder = 0
      OnChange = LeftPowerBarChange
    end
    object LeftButton: TButton
      Left = 16
      Top = 24
      Width = 33
      Height = 25
      Caption = 'Off'
      Enabled = False
      TabOrder = 1
      OnClick = LeftButtonClick
    end
  end
  object RightGroupBox: TGroupBox
    Left = 8
    Top = 88
    Width = 265
    Height = 65
    Caption = 'Right Motor'
    TabOrder = 3
    object RightPowerLabel: TLabel
      Left = 243
      Top = 28
      Width = 6
      Height = 13
      Alignment = taRightJustify
      Caption = '0'
    end
    object RightPowerBar: TScrollBar
      Left = 64
      Top = 24
      Width = 153
      Height = 17
      LargeChange = 10
      Max = 254
      PageSize = 0
      TabOrder = 0
      OnChange = RightPowerBarChange
    end
    object RightButton: TButton
      Left = 16
      Top = 24
      Width = 33
      Height = 25
      Caption = 'Off'
      Enabled = False
      TabOrder = 1
      OnClick = RightButtonClick
    end
  end
  object HidCtl: TJvHidDeviceController
    Left = 104
    Top = 176
  end
end
