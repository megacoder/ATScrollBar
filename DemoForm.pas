unit DemoForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATScrollBar, ComCtrls;

type
  TFormDemo = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    chkDraw: TCheckBox;
    trackBor: TTrackBar;
    Label1: TLabel;
    labv: TLabel;
    labh: TLabel;
    Label2: TLabel;
    trackPage: TTrackBar;
    Label3: TLabel;
    trackSize: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure chkDrawClick(Sender: TObject);
    procedure trackBorChange(Sender: TObject);
    procedure trackPageChange(Sender: TObject);
    procedure trackSizeChange(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeH(S: TObject);
    procedure ChangeV(S: TObject);
    procedure DrawEvent(S: TObject; AType: TATScrollElemType;
      ACanvas: TCanvas; const ARect: TRect; var ACanDo: boolean);
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
  bh.OnChange:= ChangeH;
  bh.Min:= 20;
  bh.Max:= 200;

  //-----------------------------------
  bv:= TATScroll.Create(Self);
  bv.Parent:= Panel1;
  bv.Align:= alRight;
  bv.Kind:= sbVertical;
  bv.OnChange:= ChangeV;
  bv.Min:= 10;
  bv.Max:= 100;

  bv.Width:= 22;
  bh.Height:= bv.Width;
  bh.IndentRight:= bv.Width;
end;

procedure TFormDemo.DrawEvent;
const
  cc: array[TATScrollElemType] of TColor = (
    clYellow, clYellow,
    clLime, clLime,
    clCream, clGreen, $e0a0c0, clNavy, $e05050);
begin
  ACanvas.Brush.Color:= cc[AType];
  ACanvas.FillRect(ARect);
  ACanDo:= false;
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

procedure TFormDemo.trackBorChange(Sender: TObject);
begin
  bv.IndentBorder:= trackBor.Position;
  bh.IndentBorder:= bv.IndentBorder;
  bv.Invalidate;
  bh.Invalidate;
end;

procedure TFormDemo.ChangeH(S: TObject);
begin
  labh.Caption:= Format('Horz %d (%d .. %d)', [bh.Position, bh.Min, bh.Max]);
end;

procedure TFormDemo.ChangeV(S: TObject);
begin
  labv.Caption:= Format('Vert %d (%d .. %d)', [bv.Position, bv.Min, bv.Max]);
end;

procedure TFormDemo.trackPageChange(Sender: TObject);
begin
  bv.PageSize:= trackPage.Position;
  bh.PageSize:= bv.PageSize;
end;

procedure TFormDemo.trackSizeChange(Sender: TObject);
var
  n: Integer;
begin
  n:= trackSize.Position;
  bv.IndentArrow:= n div 5;
  bh.IndentArrow:= bv.IndentArrow;
  bv.Width:= n;
  bh.IndentRight:= n;
  bh.Height:= n;
end;

end.
