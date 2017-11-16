unit uTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Math, uCoordinates;

type
  TTool = class
  private
    mButton: TMouseButton;
    mDoublePoints: array of TDoublePoint;
  public
    constructor Create(x, y: Double; Button: TMouseButton);
    procedure Update(x, y: Integer); virtual; abstract;
    procedure MouseUp(x, y: Integer); virtual;
  end;

  TToolClass = class of TTool;

   THand = class(TTool)
   public
    procedure Update(x, y: Integer); override;
    procedure MouseUp(x, y: Integer); override;
  end;

  TLoupe = class(TTool)
  public
    procedure Update(x, y: Integer); override;
    procedure MouseUp(x, y: Integer); override;
  end;

procedure registerTools(ToolClasses: array of TToolClass);

var
  gToolClasses: array of TToolClass;

implementation

constructor TTool.Create(x, y: Double; Button: TMouseButton);
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

procedure TTool.MouseUp(x, y: Integer);
begin
//
end;

procedure THand.Update(x, y: Integer);
begin
  mDoublePoints[1] := CanvasToWorld(x, y);
  gCanvasOffset.mX := gCanvasOffset.mX + mDoublePoints[0].mX - mDoublePoints[1].mX;
  gCanvasOffset.mY := gCanvasOffset.mY + mDoublePoints[0].mY - mDoublePoints[1].mY;


end;

procedure THand.MouseUp(x, y: Integer);
begin
//
end;

procedure TLoupe.Update(x, y: Integer);
begin
  if mButton = mbLeft then
    mDoublePoints[1] := CanvasToWorld(x, y);
end;

procedure TLoupe.MouseUp(x, y: Integer);
var
  TopLeft, BottomRight: TDoublePoint;
  NewScale: double;
begin
  inherited MouseUp(X, Y);

  TopLeft := DoubleToPoint(Min(mDoublePoints[0].mX, mDoublePoints[1].mX),
                          Min(mDoublePoints[0].mY, mDoublePoints[1].mY));

  BottomRight := DoubleToPoint(Max(mDoublePoints[0].mX, mDoublePoints[1].mX),
                              Max(mDoublePoints[0].mY, mDoublePoints[1].mY));

  if mButton = mbLeft then
    ZoomPoint(CanvasToWorld(X, Y), gScale * 2)
  else if mButton = mbRight then
    ZoomPoint(CanvasToWorld(X, Y), gScale / 2);
end;


initialization

registerTools([THand, TLoupe]);
end.




