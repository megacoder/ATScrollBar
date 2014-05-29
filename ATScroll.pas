unit ATScroll;

interface

{$ifndef FPC}
{$define windows}
{$endif}

uses
  {$ifdef windows}
  Windows, Messages,
  {$endif}
  {$ifdef FPC}
  LCLIntf,
  {$endif}
  Classes, Types, Graphics,
  Controls, ExtCtrls, Forms;

type
  TATScrollArrowType = (saLeft, saRight, saTop, saBottom);
  TATScrollDrawEvent = procedure (Sender: TObject; AIndex: Integer;
    ACanvas: TCanvas; const ARect: TRect; var ACanDraw: boolean) of object;

type
  TATScroll = class(TPanel)
  private
    FKind: TScrollBarKind;
    FIndentBorder: Integer;
    FIndentRight: Integer;
    FIndentA: Integer;

    FColorBorder: TColor;
    FColorRect: TColor;
    FColorFill: TColor;
    FColorArrow: TColor;

    FPos,
    FMin,
    FMax,
    FPage: Integer;

    //internal
    FIn: TRect; //area for scrolling
    FInUp: TRect; //area for up or left arrow
    FInDown: TRect; //area for down or right arrow
    FInThumb: TRect; //area for scroll-thumb
    FBitmap: TBitmap;

    FOnPanelDrawBefore: TATScrollDrawEvent;
    FOnPanelDrawAfter: TATScrollDrawEvent;

    procedure DoPaintArrow(C: TCanvas; R: TRect; Typ: TATScrollArrowType);
    procedure DoPaintThumb(C: TCanvas);
    procedure DoPaintTo(C: TCanvas);
    function IsHorz: boolean;
    function GetPxAtScroll(APos: Integer): Integer;

    procedure SetKind(Value: TScrollBarKind);
    procedure SetPos(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPage(Value: Integer);
    function DoDrawBefore(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
    function DoDrawAfter(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
    property Position: Integer read FPos write SetPos;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property PageSize: Integer read FPage write SetPage;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Click; override;
    {$ifdef windows}
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    {$endif}
  published
    property Kind: TScrollBarKind read FKind write SetKind;
    property IndentBorder: Integer read FIndentBorder write FIndentBorder;
    property IndentRight: Integer read FIndentRight write FIndentRight;
    property IndentArrow: Integer read FIndentA write FIndentA;
    property ColorBorder: TColor read FColorBorder write FColorBorder;
    property OnPanelDrawBefore: TATScrollDrawEvent read FOnPanelDrawBefore write FOnPanelDrawBefore;
    property OnPanelDrawAfter: TATScrollDrawEvent read FOnPanelDrawAfter write FOnPanelDrawAfter;
  end;

implementation

uses
  SysUtils, Math;

{ TATScroll }

constructor TATScroll.Create(AOnwer: TComponent);
begin
  inherited;

  Caption:= '';
  BorderStyle:= bsNone;
  ControlStyle:= ControlStyle+[csOpaque];
  Width:= 200;
  Height:= 20;

  FKind:= sbHorizontal;
  FIndentBorder:= 1;
  FIndentRight:= 0;
  FIndentA:= 3;

  FMin:= 0;
  FMax:= 100;
  FPage:= 20;

  Color:= $E0E0E0;
  FColorBorder:= clLtGray;
  FColorArrow:= $404040;
  FColorRect:= $808080;
  FColorFill:= $c0c0c0;

  FBitmap:= TBitmap.Create;
  FBitmap.PixelFormat:= pf24bit;
  FBitmap.Width:= 1600;
  FBitmap.Height:= 60;
end;

destructor TATScroll.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TATScroll.Paint;
begin
  if Assigned(FBitmap) then
  begin
    DoPaintTo(FBitmap.Canvas);
    Canvas.CopyRect(ClientRect, FBitmap.Canvas, ClientRect);
  end;
end;

procedure TATScroll.DoPaintTo(C: TCanvas);
var
  fSize: Integer;
begin
  FIn:= ClientRect;

  if FIndentRight>0 then
  begin
    C.Brush.Color:= Color;
    C.FillRect(FIn);
    Dec(FIn.Right, FIndentRight);
  end;

  C.Brush.Color:= FColorBorder;
  C.FillRect(FIn);

  FIn:= Rect(
    FIn.Left+FIndentBorder,
    FIn.Top+FIndentBorder,
    FIn.Right-FIndentBorder,
    FIn.Bottom-FIndentBorder);

  if IsHorz then
  begin
    FSize:= Math.Min(FIn.Bottom-FIn.Top, (FIn.Right-FIn.Left) div 2);
    FInUp:= Rect(FIn.Left, FIn.Top, FIn.Left+FSize, FIn.Bottom);
    FInDown:= Rect(FIn.Right-FSize, FIn.Top, FIn.Right, FIn.Bottom);
    DoPaintArrow(C, FInUp, saLeft);
    DoPaintArrow(C, FInDown, saRight);
    Inc(FIn.Left, FSize);
    Dec(FIn.Right, FSize);
  end
  else
  begin
    FSize:= Math.Min(FIn.Right-FIn.Left, (FIn.Bottom-FIn.Top) div 2);
    FInUp:= Rect(FIn.Left, FIn.Top, FIn.Right, FIn.Top+FSize);
    FInDown:= Rect(FIn.Left, FIn.Bottom-FSize, FIn.Right, FIn.Bottom);
    DoPaintArrow(C, FInUp, saTop);
    DoPaintArrow(C, FInDown, saBottom);
    Inc(FIn.Top, FSize);
    Dec(FIn.Bottom, FSize);
  end;

  C.Brush.Color:= Color;
  C.FillRect(FIn);
  
  DoPaintThumb(C);
end;


procedure TATScroll.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
end;

procedure TATScroll.Resize;
begin
  inherited;
  if Assigned(FBitmap) then
  begin
    //little complicated to speed up
    if IsHorz then
    begin
      FBitmap.Width:= Math.Max(FBitmap.Width, Width);
      FBitmap.Height:= Height;
    end
    else
    begin
      FBitmap.Width:= Width;
      FBitmap.Height:= Math.Max(FBitmap.Height, Height);
    end;
  end;
end;


{$ifdef windows}
//needed to remove flickering on resize and mouse-over
procedure TATScroll.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result:= 1;
end;
{$endif}

procedure TATScroll.Click;
begin
  inherited;
end;

function TATScroll.DoDrawBefore(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnPanelDrawBefore) then
    FOnPanelDrawBefore(Self, AIndex, ACanvas, ARect, Result);
end;

function TATScroll.DoDrawAfter(AIndex: Integer; ACanvas: TCanvas; const ARect: TRect): boolean;
begin
  Result:= true;
  if Assigned(FOnPanelDrawAfter) then
    FOnPanelDrawAfter(Self, AIndex, ACanvas, ARect, Result);
end;

procedure TATScroll.SetKind(Value: TScrollBarKind);
begin
  if Value<>FKind then
  begin
    FKind:= Value;
    Invalidate;
  end;
end;

procedure TATScroll.DoPaintArrow(C: TCanvas; R: TRect;
  Typ: TATScrollArrowType);
var
  P, P1, P2, P3: TPoint;
  cc: Integer;
begin
  C.Brush.Color:= FColorRect;
  C.FillRect(R);

  InflateRect(R, -1, -1);
  C.Brush.Color:= FColorFill;
  C.FillRect(R);

  P:= CenterPoint(R);
  cc:= FIndentA;

  case Typ of
    saTop:
      begin
        P1:= Point(P.X-cc, P.Y+cc div 2);
        P2:= Point(P.X+cc, P.Y+cc div 2);
        P3:= Point(P.X, P.Y-cc+cc div 2);
      end;
    saBottom:
      begin
        P1:= Point(P.X-cc, P.Y-cc div 2);
        P2:= Point(P.X+cc, P.Y-cc div 2);
        P3:= Point(P.X, P.Y+cc-cc div 2);
      end;
    saLeft:
      begin
        P1:= Point(P.X+cc div 2, P.Y-cc);
        P2:= Point(P.X+cc div 2, P.Y+cc);
        P3:= Point(P.X-cc+cc div 2, P.Y);
      end;
    saRight:
      begin
        P1:= Point(P.X-cc div 2    -1, P.Y-cc);
        P2:= Point(P.X-cc div 2    -1, P.Y+cc);
        P3:= Point(P.X+cc-cc div 2 -1, P.Y);
      end;
    else
      Exit;
 end;     

  C.Brush.Color:= FColorArrow;
  C.Pen.Color:= FColorArrow;
  C.Polygon([P1, P2, P3]);
end;

function TATScroll.IsHorz: boolean;
begin
  Result:= FKind=sbHorizontal;
end;

function TATScroll.GetPxAtScroll(APos: Integer): Integer;
var
  N0, NLen: Integer;
begin
  if IsHorz then
  begin
    N0:= FIn.Left;
    NLen:= FIn.Right-FIn.Left
  end
  else
  begin
    N0:= FIn.Top;
    NLen:= FIn.Bottom-FIn.Top;
  end;
  Result:= N0 + APos * NLen div (FMax-FMin);
end;

procedure TATScroll.DoPaintThumb(C: TCanvas);
const
  cMinView = 10;
  cMinMark = cMinView*2;
  cMarkOf = 4;
var
  R: TRect;
  P: TPoint;
begin
  C.Pen.Color:= clRed;

  if IsHorz then
  begin
    R.Top:= FIn.Top;
    R.Bottom:= FIn.Bottom;
    R.Left:= GetPxAtScroll(FPos);
    R.Left:= Math.Min(R.Left, FIn.Right-cMinView);
    R.Right:= GetPxAtScroll(FPos+FPage);
    R.Right:= Math.Max(R.Right, R.Left+cMinView);
    R.Right:= Math.Min(R.Right, FIn.Right);
  end
  else
  begin
    R.Left:= FIn.Left;
    R.Right:= FIn.Right;
    R.Top:= GetPxAtScroll(FPos);
    R.Top:= Math.Min(R.Top, FIn.Bottom-cMinView);
    R.Bottom:= GetPxAtScroll(FPos+FPage);
    R.Bottom:= Math.Max(R.Bottom, R.Top+cMinView);
    R.Bottom:= Math.Min(R.Bottom, FIn.Bottom);
  end;
  FInThumb:= R;
  
  C.Brush.Color:= FColorFill;
  C.Pen.Color:= FColorRect;
  C.Rectangle(R);

  P:= CenterPoint(R);
  if IsHorz then
  begin
    if (R.Right-R.Left)>cMinMark then
    begin
      C.MoveTo(P.X, FIn.Top+cMarkOf);
      C.LineTo(P.X, FIn.Bottom-cMarkOf);
      C.MoveTo(P.X-2, FIn.Top+cMarkOf);
      C.LineTo(P.X-2, FIn.Bottom-cMarkOf);
      C.MoveTo(P.X+2, FIn.Top+cMarkOf);
      C.LineTo(P.X+2, FIn.Bottom-cMarkOf);
    end;
  end
  else
  begin
    if (R.Bottom-R.Top)>cMinMark then
    begin
      C.MoveTo(FIn.Left+cMarkOf, P.Y);
      C.LineTo(FIn.Right-cMarkOf, P.Y);
      C.MoveTo(FIn.Left+cMarkOf, P.Y-2);
      C.LineTo(FIn.Right-cMarkOf, P.Y-2);
      C.MoveTo(FIn.Left+cMarkOf, P.Y+2);
      C.LineTo(FIn.Right-cMarkOf, P.Y+2);
    end;
  end;
end;


procedure TATScroll.SetMax(Value: Integer);
begin
  if FMax<>Value then
  begin
    FMax:= Value;
    Invalidate;
  end;
end;

procedure TATScroll.SetMin(Value: Integer);
begin
  if FMin<>Value then
  begin
    FMin:= Value;
    Invalidate;
  end;
end;

procedure TATScroll.SetPage(Value: Integer);
begin
  if FPage<>Value then
  begin
    FPage:= Value;
    Invalidate;
  end;
end;

procedure TATScroll.SetPos(Value: Integer);
begin
  if FPos<>Value then
  begin
    FPos:= Value;
    FPos:= Math.Min(FPos, FMax);
    FPos:= Math.Max(FPos, FMin);
    Invalidate;
  end;
end;

end.
