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
    mDoublePoints: array of TDoublePoint;
  public
    constructor Create(x, y: Double; Button: TMouseButton);
    procedure Paint(Canvas: TCanvas); virtual; abstract;
    procedure Update(x, y: Integer); virtual; abstract;
  end;

  TFigureClass = class of TFigure;

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

procedure registerFigures(FigureClasses: array of TFigureClass);

var
  gFigureClasses: array of TFigureClass;

implementation

constructor TFigure.Create(x, y: Double; Button: TMouseButton);
begin
  SetLength(mDoublePoints, 2);
  mDoublePoints[0] := DoubleToPoint(x, y);
  mDoublePoints[1] := mDoublePoints[0];
  mButton := Button;
end;

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

initialization

registerFigures([TPolyline, TLine,
                 TRectangle, TEllipse]);
end.

