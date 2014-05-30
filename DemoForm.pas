unit DemoForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATScroll, ComCtrls;

type
  TFormDemo = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    trackV: TTrackBar;
    trackH: TTrackBar;
    chkDraw: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure trackVChange(Sender: TObject);
    procedure trackHChange(Sender: TObject);
    procedure chkDrawClick(Sender: TObject);
  private
    { Private declarations }
    procedure DrawEvent(S: TObject; AType: TATScrollElemType;
      ACanvas: TCanvas; const ARect: TRect; var ACan: boolean);
  public
    { Public declarations }
    bh, bv: TATScroll;
  end;

var
  FormDemo: TFormDemo;

implementation

uses StrUtils, Math;

{$R *.dfm}

procedure TFormDemo.FormCreate(Sender: TObject);
begin
  bh:= TATScroll.Create(Self);
  bh.Parent:= Panel1;
  bh.Align:= alBottom;
  bh.Kind:= sbHorizontal;

  //-----------------------------------
  bv:= TATScroll.Create(Self);
  bv.Parent:= Panel1;
  bv.Align:= alRight;
  bv.Kind:= sbVertical;

  bv.Width:= 22;
  bh.Height:= bv.Width;
  bh.IndentRight:= bv.Width;
end;

procedure TFormDemo.trackVChange(Sender: TObject);
begin
  bv.Position:= trackV.Position;
end;

procedure TFormDemo.trackHChange(Sender: TObject);
begin
  bh.Position:= trackH.Position;
end;

procedure TFormDemo.DrawEvent;
const
  cc: array[TATScrollElemType] of TColor = (clWhite, clYellow, clLime, clPurple,
    clCream, clGreen);
begin
  ACanvas.Brush.Color:= cc[AType];
  ACanvas.FillRect(ARect);
  ACan:= false;
end;

procedure TFormDemo.chkDrawClick(Sender: TObject);
begin
  if chkDraw.Checked then
  begin
    bh.OnOwnerDraw:= DrawEvent;
    bv.OnOwnerDraw:= DrawEvent;
  end
  else
  begin
    bh.OnOwnerDraw:= nil;
    bv.OnOwnerDraw:= nil;
  end;
  bh.Invalidate;
  bv.Invalidate;
end;

end.
