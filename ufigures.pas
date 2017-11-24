unit ufigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Types, Math, uCoordinates, uProperty;

type

  TFigure = class
  private
    mButton: TMouseButton;
    mPenColor: TColor;
    mBrushColor: TColor;
    mPenWidth: Integer;
    mPenStyle: Integer;
    mBrushStyle: Integer;
    mRX, mRY: Integer;
  protected
    function GetTopLeft: TDoublePoint;
    function GetBottomRight: TDoublePoint;
  public
    mI: Integer;
    mIsSelected: boolean;
    mDoublePoints: array of TDoublePoint;
    property TopLeftBorder: TDoublePoint read GetTopLeft;
    property BottomRightBorder: TDoublePoint read GetBottomRight;
    constructor Create(x, y: Double; Button: TMouseButton);
    function IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean; virtual; abstract;
    procedure Paint(Canvas: TCanvas); virtual;
    procedure Update(x, y: Integer); virtual; abstract;
    procedure DrawFrame(Canvas: TCanvas; Figure: TFigure); virtual;
    class procedure SetParameters(panel: TPanel); virtual; abstract;
  end;

  TFigureClass = class of TFigure;

  TPolyline = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas; Figure: TFigure); override;
    class procedure SetParameters(panel: TPanel); override;
  end;

  TLine = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas; Figure: TFigure); override;
    class procedure SetParameters(panel: TPanel); override;
  end;

  //TZigZagLine = class(TFigure)
  //public
  //  mZigZagPoint: TDoublePoint;
  //  procedure Paint(Canvas: TCanvas); override;
  //  procedure Update(x, y: Integer); override;
  //  class procedure SetParameters(panel: TPanel); override;
  //end;

  TRectangle = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas; Figure: TFigure); override;
    class procedure SetParameters(panel: TPanel); override;
  end;


  TRoundRectangle = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas; Figure: TFigure); override;
    class procedure SetParameters(panel: TPanel); override;
  end;

  TEllipse = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas; Figure: TFigure); override;
    class procedure SetParameters(panel: TPanel); override;
  end;

procedure registerFigures(FigureClasses: array of TFigureClass);

var
  gFigures: array of TFigure;
  gFigureClasses: array of TFigureClass;

implementation

{TFigure}
constructor TFigure.Create(x, y: Double; Button: TMouseButton);
begin
  SetLength(mDoublePoints, 2);
  mDoublePoints[0] := DoubleToPoint(x, y);
  mDoublePoints[1] := mDoublePoints[0];
  mPenColor := TPenColor.sPenColor;
  mPenStyle := TPenStyle.sPenStyle;
  mPenWidth := TPenWidth.sPenWidth;
  mBrushColor:= TBrushColor.sBrushColor;
  mBrushStyle := TBrushStyle.sBrushStyle;
  mRX := TRoundRect.sRX;
  mRY := TRoundRect.sRY;
  mButton := Button;
  mI := 3;
end;

procedure TFigure.Paint(Canvas: TCAnvas);
begin
  with Canvas do
  begin
    Pen.Color := mPenColor;
    Pen.Width := mPenWidth;
    Brush.Color := mBrushColor;
    Pen.Style := TPenStyle.PEN_STYLES[mPenStyle].PenStyle;
    Brush.Style:= TBrushStyle.BRUSH_STYLES[mBrushStyle].BrushStyle;
  end;
end;

function TFigure.GetTopLeft: TDoublePoint;
var
  dp: TDoublePoint;
begin
  Result := mDoublePoints[0];
  for dp in mDoublePoints do
  begin
    Result.mX := min(Result.mX, dp.mX);
    Result.mY := min(Result.mY, dp.mY);
  end;
end;

function TFigure.GetBottomRight: TDoublePoint;
var
  dp : TDoublePoint;
begin
  Result := mDoublePoints[0];
  for dp in mDoublePoints do
  begin
    Result.mX := max(Result.mX, dp.mX);
    Result.mY := max(Result.mY, dp.mY);
  end;
end;

procedure TFigure.DrawFrame(Canvas: TCanvas; Figure: TFigure);
begin
  with canvas do
  begin
    Pen.Color := clBlue;
    Pen.Width := 1;
    case mI of
    0:  pen.style := psSolid;
    1:  Pen.Style := psDashDot;
    2:  Pen.Style := psDash;
    end;
    Brush.Style := bsClear;
  end;
end;

{Register figures}
procedure registerFigures(FigureClasses: array of TFigureClass);
var
  FigureCLass: TFigureClass;

begin
  for FigureClass in FigureClasses do
  begin
    SetLength(gFigureClasses, length(gFigureClasses) + 1);
    gFigureClasses[high(gFigureClasses)] := FigureClass;
  end;
end;

{Polyline}
procedure TPolyline.Paint(Canvas: TCanvas);
var
  i: Integer;
  PointsOnCanvas: array of TPoint;
begin
  inherited Paint(Canvas);
  SetLength(PointsOnCanvas, Length(mDoublePoints));
  for i := Low(mDoublePoints) to High(mDoublePoints) do
    PointsOnCanvas[i] := WorldToCanvas(mDoublePoints[i]);
  Canvas.Polyline(PointsOnCanvas);
end;

procedure TPolyLine.Update(x, y: Integer);
begin
  if mButton = mbLeft then
  begin
    SetLength(mDoublePoints, length(mDoublePoints) + 1);
    mDoublePoints[high(mDoublePoints)] := CanvasToWorld(x, y);
  end;
end;

class procedure TPolyline.SetParameters(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
end;

function TPolyline.IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean;
var
  x, y, x1, x2, y1, y2, c: Double;
  i, j, w: Integer;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  w := Figure.mPenWidth + 5;
  for i := 0 to High(mDoublePoints) do
  begin
    x1 := Min(mDoublePoints[i].mX, mDoublePoints[i + 1].mX);
    x2 := Max(mDoublePoints[i].mX, mDoublePoints[i + 1].mX);
    y1 := Min(mDoublePoints[i].mY, mDoublePoints[i + 1].mY);
    y2 := Max(mDoublePoints[i].mY, mDoublePoints[i + 1].mY);
    w := Figure.mPenWidth + 5;
    if (x1 = x2) then
    begin
      if ((y >= y1) and (y <= y2)) or ((y <= y1) and (y >= y2)) then
      begin
        Result := True;
        exit;
      end;
    end
    else
      for j := 1 to 2 do
        if (sqr(x - x1) + sqr(y - y1) <= sqr(w / 2 + 2)) or
          (sqr(x - x2) + sqr(y - y2) <= sqr(w / 2 + 2)) or
          ((y >= (y2 - y1) / (x2 - x1) * x + y2 - (y2 - y1) / (x2 - x1) * x2 - w) and
          (y <= (y2 - y1) / (x2 - x1) * x + y2 - (y2 - y1) / (x2 - x1) * x2 + w) and
          (x >= x1) and (x <= x2)) then
        begin
          Result := True;
          exit;
        end
        else
        begin
          c := y2;
          y2 := y1;
          y1 := c;
          Result := False;
        end;
  end;
end;

procedure TPolyline.DrawFrame(Canvas: TCanvas; Figure: TFigure);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas, Figure);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (Figure.mPenWidth div 2) - 5, p1.y - (Figure.mPenWidth div 2) - 5,
                  p2.x + (Figure.mPenWidth div 2) + 5, p2.y + (Figure.mPenWidth div 2) + 5);
end;

{Line}
procedure TLine.Paint(Canvas: TCanvas);
begin
  inherited Paint(Canvas);
  Canvas.Line(WorldToCanvas(mDoublePoints[0]), WOrldTOCanvas(mDoublePoints[1]));
end;

procedure TLine.Update(x, y: Integer);
begin
  if mButton = mbLeft then
    mDoublePoints[high(mDoublePoints)] := CanvasToWorld(x, y);
end;

class procedure TLine.SetParameters(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
end;

function TLine.IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
  c: Double;
  i, w: Integer;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := gFigures[num].TopLeftBorder.mX;
  y1 := gFigures[num].TopLeftBorder.mY;
  x2 := gFigures[num].BottomRightBorder.mX;
  y2 := gFigures[num].BottomRightBorder.mY;
  w := Figure.mPenWidth + 2;
  if (x1 = x2) then
   begin
    if((y >= y1) and (y <= y2)) or ((y <= y1) and (y >= y2))
      then Result := true;
   end
  else
   begin
      //a := abs((y1 - y2) / (x1 - x2));
      //b := abs((y1 + y2) - a * (x1 + x2)) / 2;
      //if ((round(y) = round(a * x + b)) and (x > x1) and (x < x2)) or ((round(y) = round(a * x + b)) and (x2 > x1) and (x < x1)) then
      //if (((y - y1) / (y2 - y1)) = ((x -x1) / (x2 - x1))) and (((x >= x1) and (x2 >= x)) or ((x >= x2) and (x1 >= x))) then
      //if ((y1 - y2) * (x - x1) + (x2 - x1) * (y - y1) <= 0.5 ) and (((x >= x1) and (x2 >= x)) or ((x >= x2) and (x1 >= x))) then
      // result := true;
      for i := 0 to 1 do
      if (sqr(x - x1) + sqr(y - y1) <= sqr(w / 2 + 2)) or
        (sqr(x - x2) + sqr(y - y2) <= sqr(w / 2 + 2)) or
        ((y >= (y2 - y1) / (x2 - x1) * x + y2 - (y2 - y1) / (x2 - x1) * x2 - w) and
        (y <= (y2 - y1) / (x2 - x1) * x + y2 - (y2 - y1) / (x2 - x1) * x2 + w) and
        (x >= x1) and (x <= x2)) then
      begin
        Result := True;
        break;
      end
      else
      begin
        c := y2;
        y2 := y1;
        y1 := c;
        Result := False;
      end;
   end
end;

procedure TLine.DrawFrame(Canvas: TCanvas; Figure: TFigure);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas, Figure);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (Figure.mPenWidth div 2) - 5, p1.y - (Figure.mPenWidth div 2) - 5,
                  p2.x + (Figure.mPenWidth div 2) + 5, p2.y + (Figure.mPenWidth div 2) + 5);
end;

{Zig Zag Line}
//procedure TZigZagLine.Paint(Canvas: TCanvas);
//begin
//  inherited Paint(Canvas);
//end;

//procedure TZigZagLine.Update(x, y: Integer);
//begin
//  if mButton = mbLeft then
//  begin
//  end;
//end;
//
//class procedure TZigZagLine.SetParameters(panel: TPanel);
//begin
//  CreateColorButton(Panel, 'Line color', sPenColor, @PenColorChange);
//  CreateSpinEdit(Panel, 'Line width', sPenWidth, @PenWidthChange);
//  CreatePenStyleComboBox(panel, 'Line style', sPenStyle, @PenStyleChange);
//end;

{Rectangle}
procedure TRectangle.Paint(Canvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  inherited Paint(Canvas);
  CanvasTopLeft := WorldToCanvas(mDoublePoints[0].mX, mDoublePoints[0].mY);
  CanvasBottomRight := WorldToCanvas(mDoublePoints[1].mX, mDoublePoints[1].mY);
  Canvas.Rectangle(CanvasTopLeft.x, CanvasTopLeft.y,
                    CanvasBottomRight.x, CanvasBottomRight.y);
end;

procedure TRectangle.Update(x, y: Integer);
begin
  if mButton = mbLeft then
    mDoublePoints[high(mDoublePoints)] := CanvasToWorld(x, y);
end;

class procedure TRectangle.SetParameters(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(panel);
  TBrushColor.CreateBrushColorButton(panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
end;

function TRectangle.IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := gFigures[num].TopLeftBorder.mX;
  y1 := gFigures[num].TopLeftBorder.mY;
  x2 := gFigures[num].BottomRightBorder.mX;
  y2 := gFigures[num].BottomRightBorder.mY;
  if (x <= x2) and (x >= x1) and (y <= y2) and (y >= y1) then
      Result := true;
end;

procedure TRectangle.DrawFrame(Canvas: TCanvas; Figure: TFigure);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas, Figure);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (Figure.mPenWidth div 2) - 5, p1.y - (Figure.mPenWidth div 2) - 5,
                p2.x + (Figure.mPenWidth div 2) + 5, p2.y + (Figure.mPenWidth div 2) + 5);
end;

{Round Rectangle}
procedure TRoundRectangle.Paint(Canvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  inherited Paint(Canvas);
  CanvasTopLeft := WorldToCanvas(mDoublePoints[0].mX, mDoublePoints[0].mY);
  CanvasBottomRight := WorldToCanvas(mDoublePoints[1].mX, mDoublePoints[1].mY);
  Canvas.RoundRect(CanvasTopLeft.x, CanvasTopLeft.y,
                  CanvasBottomRight.x, CanvasBottomRight.y, mRX, mRY);
end;

procedure TRoundRectangle.Update(x, y: Integer);
begin
  if mButton = mbLeft then
    mDoublePoints[high(mDoublePoints)] := CanvasToWorld(x, y);
end;

class procedure TRoundRectangle.SetParameters(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(panel);
  TBrushColor.CreateBrushColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TRoundRect.CreateRXSpinEdit(panel);
  TRoundRect.CreateRYSpinEdit(panel);
  TPenStyle.CreatePenStyleComboBox(panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
end;

function TRoundRectangle.IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
  round: Integer;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := gFigures[num].TopLeftBorder.mX;
  y1 := gFigures[num].TopLeftBorder.mY;
  x2 := gFigures[num].BottomRightBorder.mX;
  y2 := gFigures[num].BottomRightBorder.mY;
  round := (mRX + mRY) div 2;
  if ((x >= x1) and (x <= x2) and (y >= y1 + round) and (y <= y2 - round)) or
      ((x >= x1 + round) and (x <= x2 - round) and (y >= y1) and (y <= y2)) or
      (sqr(x - x1 - round) + sqr(y - y1 - round) <= sqr(round)) or
      (sqr(x - x2 + round) + sqr(y - y1 - round) <= sqr(round)) or
      (sqr(x - x1 - round) + sqr(y - y2 + round) <= sqr(round)) or
      (sqr(x - x2 + round) + sqr(y - y2 + round) <= sqr(round))
      then Result := true;
end;

procedure TRoundRectangle.DrawFrame(Canvas: TCanvas; Figure: TFigure);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas, Figure);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (mPenWidth div 2) - 5, p1.y - (mPenWidth div 2) - 5,
                    p2.x + (mPenWidth div 2) + 5, p2.y + (mPenWidth div 2) + 5);
end;

{Ellipse}
procedure TEllipse.Paint(Canvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  inherited Paint(Canvas);
  CanvasTopLeft := WorldToCanvas(mDoublePoints[0].mX, mDoublePoints[0].mY);
  CanvasBottomRight := WorldToCanvas(mDoublePoints[1].mX, mDoublePoints[1].mY);
  Canvas.Ellipse(CanvasTopLeft.x, CanvasTopLeft.y,
                  CanvasBottomRight.x, CanvasBottomRight.y);
end;

procedure TEllipse.Update(x, y: Integer);
begin
  if mButton = mbLeft then
    mDoublePoints[high(mDoublePoints)] := CanvasToWorld(x, y);
end;

class procedure TEllipse.SetParameters(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(panel);
  TBrushColor.CreateBrushColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
end;

function TEllipse.IsPointInhere(dp: TDoublePoint; num: Integer; Figure: TFigure): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := gFigures[num].TopLeftBorder.mX;
  y1 := gFigures[num].TopLeftBorder.mY;
  x2 := gFigures[num].BottomRightBorder.mX;
  y2 := gFigures[num].BottomRightBorder.mY;
  if ((x1 - x2) <> 0) and ((y1 - y2) <> 0) then
  begin
   if (sqr(x - ((x1 + x2) / 2)) / sqr((x1 - x2) / 2) +
       sqr(y - ((y1 + y2) / 2)) / sqr((y1 - y2) / 2)) <= 1 then
       Result := true;
  end;
end;

procedure TEllipse.DrawFrame(Canvas: TCanvas; Figure: TFigure);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas, Figure);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (mPenWidth div 2) - 5, p1.y - (mPenWidth div 2) - 5,
                  p2.x + (mPenWidth div 2) + 5, p2.y + (mPenWidth div 2) + 5);
end;

initialization

registerFigures([TPolyline, TLine, TRectangle,
                 TRoundRectangle, TEllipse]);
end.

