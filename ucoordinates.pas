unit uCoordinates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDoublePoint = record
    mX, mY: Double;
  end;

var
  gScale: Double;
  gCanvasOffset: TDoublePoint;
  gCanvasWidth, gCanvasHeight: Integer;

function DoubleToPoint(x, y: Double): TDoublePoint;
function CanvasToWorld(x, y: Integer): TDoublePoint;
function CanvasToWorld(Point: TPoint): TDoublePoint;
function WorldToCanvas(x, y: Double): TPoint;
function WorldToCanvas(DoublePoint: TDoublePoint): TPoint;

procedure SetScale(scale: Double);
procedure ZoomPoint(Point: TDoublePoint; scale: Double);

implementation

function DoubleToPoint(x, y: Double):TDoublePoint;
begin
  result.mX := x;
  result.mY := y;
end;

function CanvasToWorld(x, y: Integer): TDoublePoint;
begin
  result.mX := (x / gScale + gCanvasOffset.mX);
  result.mY := (y / gScale + gCanvasOffset.mY);
end;

function CanvasToWorld(Point: TPoint): TDoublePoint;
begin
  result := CanvasToWorld(Point.x, Point.y);;
end;

function WorldToCanvas(x, y: Double): TPoint;
begin
  result.x:= Round((x - gCanvasOffset.mX) * gScale);
  result.y:= Round((y - gCanvasOffset.mY) * gScale);
end;

Function WorldToCanvas(DoublePoint: TDoublePoint): TPoint;
begin
  result := WorldToCanvas(DoublePoint.mX, DoublePoint.mY);
end;

procedure ZoomPoint(Point: TDoublePoint; scale: Double);
var
  CanvasCorner: TDoublePoint;
begin
  setScale(scale);
  CanvasCorner := CanvasToWorld(gCanvasWidth, gCanvasHeight);
  gCanvasOffset.mX := Point.mX - (CanvasCorner.mX - gCanvasOffset.mX) / 2;
  gCanvasOffset.mY := Point.mY - (CanvasCorner.mY - gCanvasOffset.mY) / 2;
end;

procedure SetScale(scale: Double);
const
  MIN_ZOOM = 0.01;
  MAX_ZOOM = 32.00;
begin
  if scale >= MAX_ZOOM then
    gScale := MAX_ZOOM
  else if scale <= MIN_ZOOM then
    gScale := MIN_ZOOM
  else gScale := scale;
end;

initialization
  gScale := 1.00;
  gCanvasOffset := DoubleToPoint(0, 0);

end.

