unit uTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Math, uFigures, uCoordinates;

type

  TToolClass = class of TTool;

  TTool = class
  private
    mButton: TMouseButton;
    mDoublePoints: array of TDoublePoint;
  public
    mIsActive: boolean;
    constructor Create(x, y: double; Button: TMouseButton);
    procedure Update(x, y: integer); virtual; abstract;
    procedure MouseUp(x, y: integer; Shift: TShiftState); virtual;
    procedure DrawArea(canvas: TCanvas); virtual;
  end;

  THand = class(TTool)
  public
    procedure Update(x, y: integer); override;
    procedure MouseUp(x, y: integer; Shift: TShiftState); override;
    procedure DrawArea(canvas: TCanvas); override;
  end;

  TLoupe = class(TTool)
  public
    procedure Update(x, y: integer); override;
    procedure MouseUp(x, y: integer; Shift: TShiftState); override;
    procedure DrawArea(canvas: TCanvas); override;
  end;

  TSelection = class(TTool)
  private
  public
    procedure Update(x, y: integer); override;
    procedure MouseUP(x, y: integer; Shift: TShiftState); override;
    procedure DrawArea(canvas: TCanvas); override;
  end;

procedure registerTools(ToolClasses: array of TToolClass);

var
  gTools: array of TTool;
  gToolClasses: array of TToolClass;

implementation

{constructor}
constructor TTool.Create(x, y: double; Button: TMouseButton);
begin
  SetLength(mDoublePoints, 2);
  mDoublePoints[0] := DoubleToPoint(x, y);
  mDoublePoints[1] := mDoublePoints[0];
  mButton := Button;
  mIsActive := true;
end;

{Register tools}
procedure registerTools(ToolClasses: array of TToolClass);
var
  ToolClass: TToolClass;

begin
  for ToolClass in ToolClasses do
  begin
    SetLength(gToolClasses, length(gToolClasses) + 1);
    gToolClasses[high(gToolClasses)] := ToolClass;
  end;
end;

{TTool}
procedure TTool.MouseUp(x, y: integer; shift: TShiftState);
begin
//
end;

procedure TTool.DrawArea(canvas: TCanvas);
begin
  with canvas do
  begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Color := clBlue;
    Brush.Style := bsClear;
  end;
end;

{Hand}
procedure THand.Update(x, y: integer);
begin
  mDoublePoints[1] := CanvasToWorld(x, y);
  gCanvasOffset.mX := gCanvasOffset.mX + mDoublePoints[0].mX - mDoublePoints[1].mX;
  gCanvasOffset.mY := gCanvasOffset.mY + mDoublePoints[0].mY - mDoublePoints[1].mY;

end;

procedure THand.MouseUp(x, y: integer; Shift: TShiftState);
begin
//
end;

procedure THand.DrawArea(canvas: TCanvas);
begin
  //
end;

{Loupe}
procedure TLoupe.Update(x, y: integer);
begin
  if mButton = mbLeft then
    mDoublePoints[1] := CanvasToWorld(x, y);
end;

procedure TLoupe.MouseUp(x, y: integer; Shift: TShiftState);
begin
  inherited MouseUp(X, Y, shift);
  if mButton = mbLeft then
    ZoomPoint(CanvasToWorld(X, Y), gScale * 2)
  else if mButton = mbRight then
    ZoomPoint(CanvasToWorld(X, Y), gScale / 2);
end;

procedure TLoupe.DrawArea(canvas: TCanvas);
begin
  //
end;

{Selection}
procedure TSelection.Update(x, y: integer);
begin
  if mButton = mbLeft then
  begin
    mDoublePoints[1] := CanvasToWorld(x, y);
  end;
end;

procedure TSelection.MouseUp(x, y: integer; Shift: TShiftState);
var
  dp: TDoublePoint;
  i: integer;
  x1, x2, y1, y2: Integer;
  p1, p2: TPoint;
const
  EPS = 2;
begin
  p1 := WorldToCanvas(mDoublePoints[0]);
  p2 := WorldToCanvas(mDoublePoints[1]);
  x1 := p1.x;
  y1 := p1.y;
  x2 := p2.x;
  y2 := p2.y;
  dp := CanvasToWorld(x, y);
  if (abs((sqr(x1) + sqr(y1)) - (sqr(x2) + sqr(y2))) <= EPS) then
  begin
    if (shift <> [ssCtrl]) then
      for i := 0 to high(gFigures) do
        gFigures[i].mIsSelected := false;

    for i := high(gFigures) downto 0 do
    begin
      if (gFigures[i].IsPointInhere(dp, i, gFigures[i])) then
      begin
        gFigures[i].mIsSelected := true;
        Break;
      end;
    end;
  end
  else
  begin
      for i := 0 to high(gFigures) do
        gFigures[i].mIsSelected := false;
    for i := high(gFigures) downto 0 do
    begin
        if (gFigures[i].TopLeftBorder.mX <= max(x1, x2)) and (gFigures[i].TopLeftBorder.mX >= min(x1, x2)) and
            (gFigures[i].TopLeftBorder.mY <= max(y1, y2)) and (gFigures[i].TopLeftBorder.mY >= min(y1, y2)) and
            (gFigures[i].BottomRightBorder.mX <= max(x1,x2)) and (gFigures[i].BottomRightBorder.mX >= min(x1, x2)) and
            (gFigures[i].BottomRightBorder.mY <= max(y1, y2)) and (gFigures[i].BottomRightBorder.mY >= min(y1, y2))
            then gFigures[i].mIsSelected := true;
    end;
  end;
  mIsActive := false;
end;

procedure TSelection.DrawArea(canvas: TCanvas);
var
  x1, x2, y1, y2: Integer;
  p1, p2: TPoint;
begin
  inherited DrawArea(canvas);
  p1 := WorldToCanvas(mDoublePoints[0]);
  p2 := WorldToCanvas(mDoublePoints[1]);
  x1 := p1.x;
  y1 := p1.y;
  x2 := p2.x;
  y2 := p2.y;
  canvas.Rectangle(x1, y1, x2, y2);
end;

initialization

  registerTools([THand, TLoupe, TSelection]);
end.






