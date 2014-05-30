object FormDemo: TFormDemo
  Left = 303
  Top = 282
  AutoScroll = False
  Caption = 'ATScrollBar demo'
  ClientHeight = 381
  ClientWidth = 716
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 257
    Top = 0
    Width = 9
    Height = 381
    AutoSnap = False
    Beveled = True
    ResizeStyle = rsUpdate
  end
  object Panel1: TPanel
    Left = 266
    Top = 0
    Width = 450
    Height = 381
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 160
      Width = 53
      Height = 13
      Caption = 'Border size'
    end
    object labv: TLabel
      Left = 24
      Top = 32
      Width = 6
      Height = 13
      Caption = 'V'
    end
    object labh: TLabel
      Left = 24
      Top = 48
      Width = 7
      Height = 13
      Caption = 'H'
    end
    object chkDraw: TCheckBox
      Left = 16
      Top = 184
      Width = 153
      Height = 17
      Caption = 'Owner-draw'
      TabOrder = 0
      OnClick = chkDrawClick
    end
    object trackBor: TTrackBar
      Left = 112
      Top = 152
      Width = 113
      Height = 33
      Max = 5
      PageSize = 20
      TabOrder = 1
      OnChange = trackBorChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 257
    Height = 381
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
  end
end
