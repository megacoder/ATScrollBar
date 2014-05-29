unit DemoForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ATScroll, ComCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    trackV: TTrackBar;
    trackH: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure trackVChange(Sender: TObject);
    procedure trackHChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    bh, bv: TATScroll;
  end;

var
  Form1: TForm1;

implementation

uses StrUtils, Math;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
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

procedure TForm1.trackVChange(Sender: TObject);
begin
  bv.Position:= trackV.Position;
  bv.Invalidate;
end;

procedure TForm1.trackHChange(Sender: TObject);
begin
  bh.Position:= trackH.Position;
end;

end.
