unit uCoordinates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDoublePoint = record
    mX, mY: double;
  end;

var
  gScale: double;
  gCanvasOffset: TDoublePoint;
  gCanvasWidth, gCanvasHeight: integer;

function DoubleToPoint(x, y: double): TDoublePoint;
function CanvasToWorld(x, y: integer): TDoublePoint;
function CanvasToWorld(Point: TPoint): TDoublePoint;
function WorldToCanvas(x, y: double): TPoint;
function WorldToCanvas(DoublePoint: TDoublePoint): TPoint;

procedure SetScale(scale: double);
procedure ZoomPoint(Point: TDoublePoint; scale: double);

implementation

function DoubleToPoint(x, y: double): TDoublePoint;
begin
  Result.mX := x;
  Result.mY := y;
end;

function CanvasToWorld(x, y: integer): TDoublePoint;
begin
  Result.mX := (x / gScale + gCanvasOffset.mX);
  Result.mY := (y / gScale + gCanvasOffset.mY);
end;

function CanvasToWorld(Point: TPoint): TDoublePoint;
begin
  Result := CanvasToWorld(Point.x, Point.y);
  ;
end;

function WorldToCanvas(x, y: double): TPoint;
begin
  Result.x := Round((x - gCanvasOffset.mX) * gScale);
  Result.y := Round((y - gCanvasOffset.mY) * gScale);
end;

function WorldToCanvas(DoublePoint: TDoublePoint): TPoint;
begin
  Result := WorldToCanvas(DoublePoint.mX, DoublePoint.mY);
end;

procedure ZoomPoint(Point: TDoublePoint; scale: double);
var
  CanvasCorner: TDoublePoint;
begin
  setScale(scale);
  CanvasCorner := CanvasToWorld(gCanvasWidth, gCanvasHeight);
  gCanvasOffset.mX := Point.mX - (CanvasCorner.mX - gCanvasOffset.mX) / 2;
  gCanvasOffset.mY := Point.mY - (CanvasCorner.mY - gCanvasOffset.mY) / 2;
end;

procedure SetScale(scale: double);
const
  MIN_ZOOM = 0.01;
  MAX_ZOOM = 32.00;
begin
  if scale >= MAX_ZOOM then
    gScale := MAX_ZOOM
  else if scale <= MIN_ZOOM then
    gScale := MIN_ZOOM
  else
    gScale := scale;
end;

initialization
  gScale := 1.00;
  gCanvasOffset := DoubleToPoint(0, 0);

end.
