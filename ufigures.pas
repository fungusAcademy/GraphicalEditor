unit ufigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Math, uCoordinates;

type

  TFigure = class
  private
    mButton: TMouseButton;
    //mPoints: array of TPoint;
    mDoublePoints: array of TDoublePoint;
    function GetTopLeft: TDoublePoint;
    function GetBottomRight: TDoublePoint;
  public
    property TopLeftBorder: TDoublePoint read GetTopLeft;
    property BottomRightBorder: TDoublePoint read GetBottomRight;
    constructor Create(x, y: Double; Button: TMouseButton);
    procedure Paint(Canvas: TCanvas); virtual; abstract;
    procedure Update(x, y: Integer); virtual; abstract;
    procedure MouseUp(x, y: Integer); virtual;
  end;

  TFigureClass = class of TFigure;

  THand = class(TFIgure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure MouseUp(x, y: Integer); override;
  end;

  TLoupe = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure MouseUp(x, y: Integer); override;
  end;

  TPolyline = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
  end;

  TLine = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
  end;

  TRectangle = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
  end;

  TEllipse = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
  end;

implementation

constructor TFigure.Create(x, y: Double; Button: TMouseButton);
begin
  SetLength(mDoublePoints, 2);
  mDoublePoints[0] := DoubleToPoint(x, y);
  mDoublePoints[1] := mDoublePoints[0];
  mButton := Button;
end;

function TFigure.GetTopLeft: TDoublePoint;
var
  DoublePoint: TDoublePoint;
begin
  result := mDoublePoints[0];
  for DoublePoint in mDoublePoints do
  begin
    result.mX := max(result.mX, DoublePoint.mX);
    result.mY := max(result.mY, DoublePoint.mY);
  end;
end;

function TFigure.GetBottomRight: TDoublePoint;
var
  DoublePoint: TDoublePoint;
begin
  result := mDoublePoints[0];
  for DoublePoint in mDoublePoints do
  begin
    result.mX := min(result.mX, DoublePoint.mX);
    result.mY := min(result.mY, DoublePoint.mY);
  end;
end;

procedure TFigure.MouseUp(x, y: Integer);
begin
//
end;

procedure THand.Update(x, y: Integer);
begin
  mDoublePoints[1] := CanvasToWorld(x, y);
  gCanvasOffset.mX := gCanvasOffset.mX + mDoublePoints[0].mX - mDoublePoints[1].mX;
  gCanvasOffset.mY := gCanvasOffset.mY + mDoublePoints[0].mY - mDoublePoints[1].mY;
end;

procedure THand.Paint(Canvas: TCanvas);
begin
//
end;

procedure THand.MouseUp(x, y: Integer);
begin
//
end;

procedure TLoupe.Update(x, y: Integer);
begin
  if mButton = mbLeft then
    mDoublePoints[1] := CanvasToWOrld(x, y);
end;

procedure TLoupe.MouseUp(x, y: Integer);
const
  eps = 16;
var
  TopLeft, BottomRight: TDoublePoint;
  NewScale: double;
begin
  inherited MouseUp(X, Y);

  TopLeft := DoubleToPoint(Min(mDoublePoints[0].mX, mDoublePoints[1].mX),
                          Min(mDoublePoints[0].mY, mDoublePoints[1].mY));

  BottomRight := DoubleToPoint(Max(mDoublePoints[0].mX, mDoublePoints[1].mX),
                              Max(mDoublePoints[0].mY, mDoublePoints[1].mY));

  if (sqr(TopLeft.mX - BottomRight.mX) + sqr(TopLeft.mY - BottomRight.mY) < eps*eps)
  then
  begin
    if mButton = mbLeft then
        ZoomPoint(CanvasToWorld(X, Y), gScale*2)
    else ZoomPoint(CanvasToWorld(X, Y), gScale/2);
  end
  else
  begin
    if (TopLeft.mX <> BottomRight.mX) and (TopLeft.mY <> BottomRight.mY) then
    begin
      NewScale := gScale*Max(gCanvasWidth / gScale / (BottomRight.mX - TopLeft.mX),
                  gCanvasHeight / gScale / (BottomRight.mY - TopLeft.mY));
      ZoomPoint(DoubleToPoint((TopLeft.mX + BottomRight.mX) / 2,
                (TopLeft.mY + BottomRight.mY) / 2), NewScale);
    end;
  end;
end;

procedure TLoupe.Paint(Canvas: TCanvas);
var
  TopLeft, BottomRight: TPoint;
begin
  TopLeft := WorldToCanvas(mDoublePoints[0]);
  BottomRight := WorldToCanvas(mDoublePoints[1]);
end;


procedure TPolyline.Paint(Canvas: TCanvas);
var
  i: Integer;
  PointsOnCanvas: array of TPoint;
begin
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

procedure TLine.Paint(Canvas: TCanvas);
begin
  Canvas.Line(WorldToCanvas(mDoublePoints[0]), WOrldTOCanvas(mDoublePoints[1]));
end;

procedure TLine.Update(x, y: Integer);
begin
  if mButton = mbLeft then
    mDoublePoints[high(mDoublePoints)] := CanvasToWorld(x, y);
end;

procedure TRectangle.Paint(Canvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  Canvas.Brush.Style:= bsClear;
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

procedure TEllipse.Paint(Canvas: TCanvas);
var
  CanvasTopLeft, CanvasBottomRight: TPoint;
begin
  Canvas.Brush.Style := bsClear;
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

begin

end.

