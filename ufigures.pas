unit ufigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Types, Math, uCoordinates, uProperty;

type

  TFigure = class
  type
    TAnchor = record
    x1, y1, x2, y2: Integer;
    index: Integer;
  end;
  private
    mButton: TMouseButton;
    //mPenColor: TColor;
    //mBrushColor: TColor;
    //mPenWidth: Integer;
    //mPenStyle: Integer;
    //mBrushStyle: Integer;
    //mRX, mRY: Integer;
  protected
    function GetTopLeft: TDoublePoint;
    function GetBottomRight: TDoublePoint;
  public
    mPenColor: TColor;
    mBrushColor: TColor;
    mPenWidth: Integer;
    mPenStyle: Integer;
    mBrushStyle: Integer;
    mRX, mRY: Integer;
    mI: Integer;
    mIsSelected: boolean;
    mIsEdited: boolean;
    mIsMoving: boolean;
    mValidProperties: array [0..6] of boolean;
    mAnchors: array of TAnchor;
    mDoublePoints: array of TDoublePoint;
    property TopLeftBorder: TDoublePoint read GetTopLeft;
    property BottomRightBorder: TDoublePoint read GetBottomRight;
    constructor Create(x, y: Double; Button: TMouseButton);
    function IsPointInhere(dp: TDoublePoint): boolean; virtual; abstract;
    procedure getParameters(); virtual; abstract;
    procedure Paint(Canvas: TCanvas); virtual;
    procedure Update(x, y: Integer); virtual; abstract;
    procedure DrawFrame(Canvas: TCanvas); virtual;
    procedure DrawAnchors(Canvas: TCanvas); virtual;
    class procedure SetStyleButtons(panel: TPanel); virtual; abstract;
    procedure MouseUp(x, y: Integer); virtual;
    procedure SetPoints(); virtual;
    procedure setStyles(); virtual;
  end;

  TFigureClass = class of TFigure;

  TPolyline = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas); override;
    procedure DrawAnchors(Canvas: TCanvas); override;
    class procedure SetStyleButtons(panel: TPanel); override;
    procedure MouseUp(x, y: Integer); override;
    procedure SetPoints(); override;
    procedure getParameters(); override;
    procedure setStyles(); override;
  end;

  TLine = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas); override;
    procedure DrawAnchors(Canvas: TCanvas); override;
    class procedure SetStyleButtons(panel: TPanel); override;
    procedure MouseUp(x, y: Integer); override;
    procedure SetPoints(); override;
    procedure getParameters(); override;
    procedure setStyles(); override;
  end;

  //TZigZagLine = class(TFigure)
  //public
  //  mZigZagPoint: TDoublePoint;
  //  procedure Paint(Canvas: TCanvas); override;
  //  procedure Update(x, y: Integer); override;
  //  class procedure SetStyleButtons(panel: TPanel); override;
  //end;

  TRectangle = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas); override;
    procedure DrawAnchors(Canvas: TCanvas); override;
    class procedure SetStyleButtons(panel: TPanel); override;
    procedure MouseUp(x, y: Integer); override;
    procedure SetPoints(); override;
    procedure getParameters(); override;
    procedure setStyles(); override;
  end;


  TRoundRectangle = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas); override;
    procedure DrawAnchors(Canvas: TCanvas); override;
    class procedure SetStyleButtons(panel: TPanel); override;
    procedure MouseUp(x, y: Integer); override;
    procedure SetPoints(); override;
    procedure getParameters(); override;
    procedure setStyles(); override;
  end;

  TEllipse = class(TFigure)
  public
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawFrame(Canvas: TCanvas); override;
    procedure DrawAnchors(Canvas: TCanvas); override;
    class procedure SetStyleButtons(panel: TPanel); override;
    procedure MouseUp(x, y: Integer); override;
    procedure SetPoints(); override;
    procedure getParameters(); override;
    procedure setStyles(); override;
  end;

procedure registerFigures(FigureClasses: array of TFigureClass);

var
  gFigures: array of TFigure;
  gFigureClasses: array of TFigureClass;

implementation

{TFigure}
constructor TFigure.Create(x, y: Double; Button: TMouseButton);
var
  i: Integer;
begin
  SetLength(mDoublePoints, 2);
  mDoublePoints[0] := DoubleToPoint(x, y);
  mDoublePoints[1] := mDoublePoints[0];
  mButton := Button;
  mI := 3;
  for i := 0 to high(gFigures)-1 do
  begin
    gFigures[i].mIsSelected := false;
    mIsEdited := false;
    mIsMoving := false;
  end;
  setStyles();
end;

procedure TFigure.Paint(Canvas: TCanvas);
begin
  with Canvas do
  begin
    Pen.Color := mPenColor;
    Pen.Width := mPenWidth;
    Brush.Color := mBrushColor;
    Pen.Style := TPenStyle.PEN_STYLES[mPenStyle].PenStyle;
    Brush.Style:= TBrushStyle.BRUSH_STYLES[mBrushStyle].BrushStyle;
    mRX := TRoundRect.sRX;
    mRY := TRoundRect.sRY;
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

procedure TFigure.DrawFrame(Canvas: TCanvas);
begin
  with canvas do
  begin
    Pen.Color := clBlue;
    Pen.Width := 1;
    Brush.Style := bsClear;
    case mI of
      0: Pen.Style := psDash;
      1: Pen.Style := psDashDot;
      2: Pen.Style := psSolid;
    end;
  end;
end;

procedure TFigure.DrawAnchors(Canvas: TCanvas);
var
  x, y, w: Integer;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  setLength(mAnchors, 7);
  w := mPenWidth div 2 + 5;

  x := WorldToCanvas(GetTopLeft).x;
  y := WorldToCanvas(GetBottomRight).y;
  Canvas.Rectangle(x-w, y-w, x+w, y+w);
  mAnchors[0].x1 := x-w;
  mAnchors[0].y1 := y-w;
  mAnchors[0].x2 := x+w;
  mAnchors[0].y2 := y+w;
  mAnchors[0].index := 0;

  x := WorldToCanvas(GetTopLeft).x;
  y := WorldToCanvas(GetTopLeft).y;
  Canvas.Rectangle(x-w, y-w, x+w, y+w);
  mAnchors[1].x1 := x-w;
  mAnchors[1].y1 := y-w;
  mAnchors[1].x2 := x+w;
  mAnchors[1].y2 := y+w;
  mAnchors[1].index := 1;

  x := WorldToCanvas(GetBottomRight).x;
  y := WorldToCanvas(GetTopLeft).y;
  Canvas.Rectangle(x-w, y-w, x+w, y+w);
  mAnchors[2].x1 := x-w;
  mAnchors[2].y1 := y-w;
  mAnchors[2].x2 := x+w;
  mAnchors[2].y2 := y+w;
  mAnchors[2].index := 2;

  x := WorldToCanvas(GetBottomRight).x;
  y := WorldToCanvas(GetBottomRight).y;
  Canvas.Rectangle(x-w, y-w, x+w, y+w);
  mAnchors[3].x1 := x-w;
  mAnchors[3].y1 := y-w;
  mAnchors[3].x2 := x+w;
  mAnchors[3].y2 := y+w;
  mAnchors[3].index := 3;

  x := WorldToCanvas(GetTopLeft).x;
  y := (WorldToCanvas(GetTopLeft).y + worldToCanvas(GetBottomRight).y) div 2;
  Canvas.Rectangle(x-w, y-w, x+w, y+w);
  mAnchors[4].x1 := x-w;
  mAnchors[4].y1 := y-w;
  mAnchors[4].x2 := x+w;
  mAnchors[4].y2 := y+w;
  mAnchors[4].index := 4;

  x := (WorldToCanvas(GetTopLeft).x + WorldToCanvas(GetBottomRight).x) div 2;
  y := WorldToCanvas(GetTopLeft).y;
  Canvas.Rectangle(x-w, y-w, x+w, y+w);
  mAnchors[5].x1 := x-w;
  mAnchors[5].y1 := y-w;
  mAnchors[5].x2 := x+w;
  mAnchors[5].y2 := y+w;
  mAnchors[5].index := 5;

  x := WorldToCanvas(GetBottomRight).x;
  y := (WorldToCanvas(GetTopLeft).y + worldToCanvas(GetBottomRight).y) div 2;
  Canvas.Rectangle(x-w, y-w, x+w, y+w);
  mAnchors[6].x1 := x-w;
  mAnchors[6].y1 := y-w;
  mAnchors[6].x2 := x+w;
  mAnchors[6].y2 := y+w;
  mAnchors[6].index := 6;

  x := (WorldToCanvas(GetTopLeft).x + WorldToCanvas(GetBottomRight).x) div 2;
  y := WorldToCanvas(GetBottomRight).y;
  Canvas.Rectangle(x-w, y-w, x+w, y+w);
  mAnchors[7].x1 := x-w;
  mAnchors[7].y1 := y-w;
  mAnchors[7].x2 := x+w;
  mAnchors[7].y2 := y+w;
  mAnchors[7].index := 7;
end;

procedure TFigure.MouseUp(x, y: Integer);
begin
//
end;

procedure TFigure.SetPoints();
var
  dp: TDoublePoint;
begin
  if (mDoublePoints[0].mX > TopLeftBorder.mX) and (mDoublePoints[0].mY > TopLeftBorder.mY) then
  begin
    dp := mDoublePoints[0];
    mDoublePoints[0] := mDoublePoints[1];
    mDoublePoints[1] := dp;
  end
  else if (mDoublePoints[0].mX > TopLeftBorder.mX) then
  begin
    dp := mDoublePoints[0];
    mDoublePoints[0].mX := mDoublePoints[1].mX;
    mDoublePoints[1].mX := dp.mX;
  end
  else if (mDoublePoints[0].mY > TopLeftBorder.mY) then
  begin
    dp := mDoublePoints[0];
    mDoublePoints[0].mY := mDoublePoints[1].mY;
    mDoublePoints[1].mY := dp.mY;
  end;
end;

procedure TFigure.setStyles();
begin
  mPenColor := TPenColor.sPenColor;
  mPenStyle := TPenStyle.sPenStyle;
  mPenWidth := TPenWidth.sPenWidth;
  mBrushColor:= TBrushColor.sBrushColor;
  mBrushStyle := TBrushStyle.sBrushStyle;
  mRX := TRoundRect.sRX;
  mRY := TRoundRect.sRY;
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

class procedure TPolyline.SetStyleButtons(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
end;

function TPolyline.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y, x1, x2, y1, y2, c: Double;
  i, j, w: Integer;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  w := mPenWidth + 5;
  for i := 0 to High(mDoublePoints) do
  begin
    // Ошибочка
    x1 := Min(mDoublePoints[i].mX, mDoublePoints[i + 1].mX);
    x2 := Max(mDoublePoints[i].mX, mDoublePoints[i + 1].mX);
    y1 := Min(mDoublePoints[i].mY, mDoublePoints[i + 1].mY);
    y2 := Max(mDoublePoints[i].mY, mDoublePoints[i + 1].mY);
    if (x1 = x2) then
    begin
      if (y >= y1) and (y <= y2) and (x <= x1 + w) and (x >= x1 - w) then
        Result := True
      else
        Result := False
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

procedure TPolyline.DrawFrame(Canvas: TCanvas);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (mPenWidth div 2) - 5, p1.y - (mPenWidth div 2) - 5,
                  p2.x + (mPenWidth div 2) + 5, p2.y + (mPenWidth div 2) + 5);
end;

procedure TPolyline.DrawAnchors(Canvas: TCanvas);
var
  x, y, i, w: Integer;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  setLength(mAnchors, high(mDoublePoints));
  w := mPenWidth div 2 + 5;
  for i := 0 to high(mDoublePoints) do
  begin
    x := WorldToCanvas(mDOublePoints[i]).x;
    y := WorldToCanvas(mDOublePoints[i]).y;
    Canvas.Rectangle(x-w, y-w, x+w, y+w);
    mAnchors[i].x1 := x-w;
    mAnchors[i].y1 := y-w;
    mAnchors[i].x2 := x+w;
    mAnchors[i].y2 := y+w;
    mAnchors[i].Index := i;
  end;
end;

procedure TPolyline.MouseUp(x, y: Integer);
begin
  inherited MouseUp(x, y);
end;

procedure TPolyline.SetPoints();
begin
  //
end;

procedure TPolyline.getParameters();
var
  i: Integer;
begin
  for i := 0 to 2 do
    mValidProperties[i] := true;
end;

procedure TPolyline.setStyles();
begin
  inherited setStyles();
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

class procedure TLine.SetStyleButtons(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
end;

procedure TLine.getParameters();
var
  i: Integer;
begin
  for i := 0 to 2 do
    mValidProperties[i] := true;
end;

function TLine.IsPointInhere(dp: TDoublePoint): boolean;
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
  x1 := Min(mDoublePoints[0].mX, mDoublePoints[1].mX);
  x2 := max(mDoublePoints[0].mX, mDoublePoints[1].mX);
  y1 := min(mDoublePoints[0].mY, mDoublePoints[1].mY);
  y2 := max(mDoublePoints[0].mY, mDoublePoints[1].mY);
  w := mPenWidth + 2;
  //if (y >= y1) and (y <= y2) and (x <= x1 + w) and (x >= x1 - w) then
  //  Result := True
  //else
  //  Result := False;
  if (x1 = x2) then
    //if ((y >= y1) and (y <= y2)) or ((y <= y1) and (y >= y2)) then
    //  Result := true
    if (y >= y1) and (y <= y2) and (x <= x1 + w) and (x >= x1 - w) then
      Result := True
    else
      Result := False
  else
   begin
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

procedure TLine.DrawFrame(Canvas: TCanvas);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (mPenWidth div 2) - 5, p1.y - (mPenWidth div 2) - 5,
                  p2.x + (mPenWidth div 2) + 5, p2.y + (mPenWidth div 2) + 5);
end;

procedure TLine.DrawAnchors(Canvas: TCanvas);
begin
  inherited DrawAnchors(canvas);
end;

procedure TLine.MouseUp(x, y: Integer);
begin
  inherited MouseUp(x, y);
end;

procedure TLine.SetPoints();
begin
  inherited SetPoints();
end;

procedure TLine.setStyles();
begin
  inherited setStyles();
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
//class procedure TZigZagLine.SetStyleButtons(panel: TPanel);
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

class procedure TRectangle.SetStyleButtons(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(panel);
  TBrushColor.CreateBrushColorButton(panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
end;

procedure TRectangle.getParameters();
var
  i: Integer;
begin
  for i := 0 to 4 do
    mValidProperties[i] := true;
end;

function TRectangle.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := TopLeftBorder.mX;
  y1 := TopLeftBorder.mY;
  x2 := BottomRightBorder.mX;
  y2 := BottomRightBorder.mY;
  if (x <= x2) and (x >= x1) and (y <= y2) and (y >= y1) then
      Result := true;
end;

procedure TRectangle.DrawFrame(Canvas: TCanvas);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (mPenWidth div 2) - 5, p1.y - (mPenWidth div 2) - 5,
                p2.x + (mPenWidth div 2) + 5, p2.y + (mPenWidth div 2) + 5);
end;

procedure TRectangle.DrawAnchors(Canvas: TCanvas);
begin
  inherited DrawAnchors(canvas);
end;

procedure TRectangle.MouseUp(x, y: Integer);
begin
  inherited MouseUp(x, y);
end;

procedure TRectangle.SetPoints();
begin
  inherited SetPoints();
end;

procedure TRectangle.setStyles();
begin
  inherited setStyles();
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

class procedure TRoundRectangle.SetStyleButtons(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(panel);
  TBrushColor.CreateBrushColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TRoundRect.CreateRXSpinEdit(panel);
  TRoundRect.CreateRYSpinEdit(panel);
  TPenStyle.CreatePenStyleComboBox(panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
end;

procedure TRoundRectangle.getParameters();
var
  i: integer;
begin
  for i := 0 to 6 do
    mValidProperties[i] := true
end;

function TRoundRectangle.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
  round: Integer;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := TopLeftBorder.mX;
  y1 := TopLeftBorder.mY;
  x2 := BottomRightBorder.mX;
  y2 := BottomRightBorder.mY;
  round := (mRX + mRY) div 2;
  if ((x >= x1) and (x <= x2) and (y >= y1 + round) and (y <= y2 - round)) or
      ((x >= x1 + round) and (x <= x2 - round) and (y >= y1) and (y <= y2)) or
      (sqr(x - x1 - round) + sqr(y - y1 - round) <= sqr(round)) or
      (sqr(x - x2 + round) + sqr(y - y1 - round) <= sqr(round)) or
      (sqr(x - x1 - round) + sqr(y - y2 + round) <= sqr(round)) or
      (sqr(x - x2 + round) + sqr(y - y2 + round) <= sqr(round))
      then Result := true;
end;

procedure TRoundRectangle.DrawFrame(Canvas: TCanvas);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (mPenWidth div 2) - 5, p1.y - (mPenWidth div 2) - 5,
                    p2.x + (mPenWidth div 2) + 5, p2.y + (mPenWidth div 2) + 5);
end;

procedure TRoundRectangle.DrawAnchors(Canvas: TCanvas);
begin
  inherited DrawAnchors(Canvas);
end;

procedure TRoundRectangle.MouseUp(x, y: Integer);
begin
  inherited MouseUp(x, y);
end;

procedure TRoundRectangle.SetPoints();
begin
  inherited SetPoints();
end;

procedure TRoundRectangle.setStyles();
begin
  inherited setStyles();
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

class procedure TEllipse.SetStyleButtons(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(panel);
  TBrushColor.CreateBrushColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
end;

procedure TEllipse.getParameters();
var
  i: integer;
begin
  for i := 0 to 4 do
    mValidProperties[i] := true
end;

function TEllipse.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := TopLeftBorder.mX; //Зависимость о ширины
  y1 := TopLeftBorder.mY;
  x2 := BottomRightBorder.mX;
  y2 := BottomRightBorder.mY;
  if ((x1 - x2) <> 0) and ((y1 - y2) <> 0) then
  begin
   if (sqr(x - ((x1 + x2) / 2)) / sqr((x1 - x2) / 2) +
       sqr(y - ((y1 + y2) / 2)) / sqr((y1 - y2) / 2)) <= 1 then
       Result := true;
  end;
end;

procedure TEllipse.DrawFrame(Canvas: TCanvas);
var
  p1, p2: TPoint;
begin
  inherited DrawFrame(canvas);
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (mPenWidth div 2) - 5, p1.y - (mPenWidth div 2) - 5,
                  p2.x + (mPenWidth div 2) + 5, p2.y + (mPenWidth div 2) + 5);
end;

procedure TEllipse.DrawAnchors(Canvas: TCanvas);
begin
  inherited DrawAnchors(canvas);
end;

procedure TEllipse.MouseUp(x, y: Integer);
begin
  inherited MouseUp(x, y);
end;

procedure TEllipse.SetPoints();
begin
  inherited SetPoints();
end;

procedure TEllipse.setStyles();
begin
  inherited setStyles();
end;

initialization

registerFigures([TPolyline, TLine, TRectangle,
                 TRoundRectangle, TEllipse]);
end.

