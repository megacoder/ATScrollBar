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
    object trackV: TTrackBar
      Left = 232
      Top = 0
      Width = 33
      Height = 177
      Max = 90
      Orientation = trVertical
      PageSize = 20
      Frequency = 10
      TabOrder = 0
      OnChange = trackVChange
    end
    object trackH: TTrackBar
      Left = 8
      Top = 168
      Width = 225
      Height = 33
      Max = 90
      PageSize = 20
      Frequency = 10
      TabOrder = 1
      OnChange = trackHChange
    end
    object chkDraw: TCheckBox
      Left = 24
      Top = 208
      Width = 153
      Height = 17
      Caption = 'Owner-draw'
      TabOrder = 2
      OnClick = chkDrawClick
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
