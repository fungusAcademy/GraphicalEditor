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
    constructor Create(x, y: double; Button: TMouseButton);
    procedure Update(x, y: integer); virtual; abstract;
    procedure MouseUp(x, y: integer; Shift: TShiftState); virtual;
  end;

  THand = class(TTool)
  public
    procedure Update(x, y: integer); override;
    procedure MouseUp(x, y: integer; Shift: TShiftState); override;
  end;

  TLoupe = class(TTool)
  public
    procedure Update(x, y: integer); override;
    procedure MouseUp(x, y: integer; Shift: TShiftState); override;
  end;

  TSelection = class(TTool)
  private
  public
    procedure Update(x, y: integer); override;
    procedure MouseUP(x, y: integer; Shift: TShiftState); override;
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
end;

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

procedure TTool.MouseUp(x, y: integer; shift: TShiftState);
begin

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

end;

{Loupe}
procedure TLoupe.Update(x, y: integer);
begin
  if mButton = mbLeft then
    mDoublePoints[1] := CanvasToWorld(x, y);
end;

procedure TLoupe.MouseUp(x, y: integer; Shift: TShiftState);
//var
//  TopLeft, BottomRight: TDoublePoint;
//  NewScale: double;
begin
  inherited MouseUp(X, Y, shift);

  //TopLeft := DoubleToPoint(Min(mDoublePoints[0].mX, mDoublePoints[1].mX),
  //                        Min(mDoublePoints[0].mY, mDoublePoints[1].mY));

  //BottomRight := DoubleToPoint(Max(mDoublePoints[0].mX, mDoublePoints[1].mX),
  //                            Max(mDoublePoints[0].mY, mDoublePoints[1].mY));

  if mButton = mbLeft then
    ZoomPoint(CanvasToWorld(X, Y), gScale * 2)
  else if mButton = mbRight then
    ZoomPoint(CanvasToWorld(X, Y), gScale / 2);
end;

{Selection}
procedure TSelection.Update(x, y: integer);
begin
  //if mButton = mbLeft then
  //  mDoublePoints[1] := CanvasToWorld(x, y);
end;

procedure TSelection.MouseUp(x, y: integer; Shift: TShiftState);
var
  dp: TDoublePoint;
  i, j: integer;
begin
  dp := CanvasToWorld(x, y);

  if (shift <> [ssCtrl]) then
    for i := 0 to high(gFigures) do
      gFigures[i].mIsSelected := false;

  for i := high(gFigures) downto 0 do
  begin
    if (gFigures[i].IsPointInhere(dp, i)) then
      begin
        gFigures[i].mIsSelected := true;
        j := i;
        Exit;
      end;
  end;
end;

initialization

  registerTools([THand, TLoupe, TSelection]);
end.






