unit uTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Math, uFigures, uCoordinates, uProperty;

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
    procedure MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel); virtual; abstract;
    procedure DrawArea(canvas: TCanvas); virtual;
    procedure MouseDown(x, y: Integer); virtual;
    class procedure SetParameters(panel: TPanel; num: Integer); virtual;
  end;

  THand = class(TTool)
  public
    procedure Update(x, y: integer); override;
    procedure MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel); override;
  end;

  TLoupe = class(TTool)
  public
    procedure Update(x, y: integer); override;
    procedure MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel); override;
  end;

  TSelection = class(TTool)
  private
  public
    procedure Update(x, y: integer); override;
    procedure MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel); override;
    procedure DrawArea(canvas: TCanvas); override;
    class procedure SetParameters(panel: TPanel; num: Integer); override;
  end;

  TEditing = class(TTool)
  public
    mIndex: Integer;
    procedure Update(x, y: integer); override;
    procedure MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel); override;
    procedure MouseDown(x, y: Integer); override;
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

procedure TTool.MouseDown(x, y: Integer);
begin
  //
end;

class procedure TTool.setParameters(panel: TPanel; num: Integer);
begin

end;

{Hand}
procedure THand.Update(x, y: integer);
begin
  mDoublePoints[1] := CanvasToWorld(x, y);
  gCanvasOffset.mX := gCanvasOffset.mX + mDoublePoints[0].mX - mDoublePoints[1].mX;
  gCanvasOffset.mY := gCanvasOffset.mY + mDoublePoints[0].mY - mDoublePoints[1].mY;

end;

procedure THand.MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel);
begin
//
end;

{Loupe}
procedure TLoupe.Update(x, y: integer);
begin
  if mButton = mbLeft then
    mDoublePoints[1] := CanvasToWorld(x, y);
end;

procedure TLoupe.MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel);
begin
  if mButton = mbLeft then
    ZoomPoint(CanvasToWorld(X, Y), gScale * 2)
  else if mButton = mbRight then
    ZoomPoint(CanvasToWorld(X, Y), gScale / 2);
end;

{Selection}
procedure TSelection.Update(x, y: integer);
begin
  if mButton = mbLeft then
  begin
    mDoublePoints[1] := CanvasToWorld(x, y);
  end;
end;

procedure TSelection.MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel);
var
  x1, x2, y1, y2, i, j, num, k: Integer;
const
  EPS = 2;
begin
  num := 0;
  k := 0;
  x1 := WorldToCanvas(mDoublePoints[0]).x;
  y1 := WorldToCanvas(mDoublePoints[0]).y;
  x2 := WorldToCanvas(mDoublePoints[1]).x;
  y2 := WorldToCanvas(mDoublePoints[1]).y;
  if (abs((sqr(x1) + sqr(y1)) - (sqr(x2) + sqr(y2))) <= EPS) then
  begin
    if (shift <> [ssCtrl]) then
      for i := 0 to high(gFigures) do
        gFigures[i].mIsSelected := false;

    for i := high(gFigures) downto 0 do
    begin
      if (gFigures[i].IsPointInhere(CanvasToWorld(x, y))) then
      begin
        gFigures[i].mIsSelected := true;
        inc(k);
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
         (gFigures[i].BottomRightBorder.mX <= max(x1, x2)) and (gFigures[i].BottomRightBorder.mX >= min(x1, x2)) and
         (gFigures[i].BottomRightBorder.mY <= max(y1, y2)) and (gFigures[i].BottomRightBorder.mY >= min(y1, y2))
      then
         begin
           gFigures[i].mIsSelected := true;
           inc(k);
         end;
    end;
  end;
  mIsActive := false;
  for i := 0 to high(gFigures) do
  begin
    if gFigures[i].mIsSelected then
    begin
      gFigures[i].getParameters();
      for j := 0 to high(gFigures[i].mValidProperties) do
        if gFigures[i].mValidProperties[j] = true then
          num := max(num, j+1);
    end;
  end;

  case k of
    0: exit;
    1: for i := 0 to high(gFigures) do
          if gFigures[i].mIsSelected then
            gFigures[i].sendStyles();
    else
      TProperty.SetDefault();
  end;

  SetParameters(panel, num);
end;

procedure TSelection.DrawArea(canvas: TCanvas);
var
  x1, x2, y1, y2: Integer;
begin
  inherited DrawArea(canvas);
  x1 := WorldToCanvas(mDoublePoints[0]).x;
  y1 := WorldToCanvas(mDoublePoints[0]).y;
  x2 := WorldToCanvas(mDoublePoints[1]).x;
  y2 := WorldToCanvas(mDoublePoints[1]).y;
  canvas.Rectangle(x1, y1, x2, y2);
end;

class procedure TSelection.SetParameters(panel: TPanel; num: Integer);
begin
  while panel.ControlCount > 0 do
    panel.Controls[0].Free;
  case num of
    3: begin
         TPenColor.CreatePenColorButton(Panel);
         TPenWidth.CreateWidthSpinEdit(Panel);
         TPenStyle.CreatePenStyleComboBox(panel);
       end;
    5: begin
         TPenColor.CreatePenColorButton(panel);
         TBrushColor.CreateBrushColorButton(panel);
         TPenWidth.CreateWidthSpinEdit(Panel);
         TPenStyle.CreatePenStyleComboBox(panel);
         TBrushStyle.CreateBrushStyleComboBox(panel);
       end;
    7: begin
         TPenColor.CreatePenColorButton(panel);
         TBrushColor.CreateBrushColorButton(Panel);
         TPenWidth.CreateWidthSpinEdit(Panel);
         TRoundRect.CreateRXSpinEdit(panel);
         TRoundRect.CreateRYSpinEdit(panel);
         TPenStyle.CreatePenStyleComboBox(panel);
         TBrushStyle.CreateBrushStyleComboBox(panel);
       end;
  end;
end;

{TEditing}
procedure TEditing.Update(x, y: integer);
var
  i, j: Integer;
  oldX, oldY, scale, prev, coeffx, coeffy: double;
begin
  mDoublePoints[1] := CanvasToWorld(x, y);
  for i := high(gFigures) downto 0 do
  begin
    if (gFigures[i].mIsEdited) then
    begin
      if high(gFigures[i].mDoublePoints) = 1 then
      begin
        case mIndex of //Криво
          0:
            begin
              gFigures[i].mDoublePoints[0].mX := gFigures[i].mDoublePoints[0].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              gFigures[i].mDoublePoints[1].mY := gFigures[i].mDoublePoints[1].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          1:
            begin
              gFigures[i].mDoublePoints[0].mX := gFigures[i].mDoublePoints[0].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              gFigures[i].mDoublePoints[0].mY := gFigures[i].mDoublePoints[0].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          2:
            begin
              gFigures[i].mDoublePoints[1].mX := gFigures[i].mDoublePoints[1].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              gFigures[i].mDoublePoints[0].mY := gFigures[i].mDoublePoints[0].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          3:
            begin
              gFigures[i].mDoublePoints[1].mX := gFigures[i].mDoublePoints[1].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              gFigures[i].mDoublePoints[1].mY := gFigures[i].mDoublePoints[1].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          4:
            begin
              gFigures[i].mDoublePoints[0].mX := gFigures[i].mDoublePoints[0].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          5:
            begin
              gFigures[i].mDoublePoints[0].mY := gFigures[i].mDoublePoints[0].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          6:
            begin
              gFigures[i].mDoublePoints[1].mX := gFigures[i].mDoublePoints[1].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          7:
            begin
              gFigures[i].mDoublePoints[1].mY := gFigures[i].mDoublePoints[1].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
        end;
      end
      else
      begin
        //for j := 0 to high(gFigures[i].mDoublePoints) do
        //  if mIndex = j then
        //  begin
        //    gFigures[i].mDoublePoints[j].mX := gFigures[i].mDoublePoints[j].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
        //    gFigures[i].mDoublePoints[j].mY := gFigures[i].mDoublePoints[j].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
        //  end;
        //mDoublePoints[0] := mDoublePoints[1];

        oldX := abs(gFigures[i].TopLeftBorder.mX - gFigures[i].BottomRightBorder.mX);
        oldY := abs(gFigures[i].TopLeftBorder.mY - gFigures[i].BottomRightBorder.mY);
        case mIndex of
          0:
            begin
              for j := 0 to high(gFigures[i].mDoublePoints) do
              begin
                  coeffX := (abs(gFigures[i].TopLeftBorder.mX - mDoublePoints[0].mX + mDoublePoints[1].mX) - gFigures[i].TopLeftBorder.mX) / oldX;
                  gFigures[i].mDoublePoints[j].mX := (gFigures[i].mDoublePoints[j].mX - gFigures[i].TopLeftBorder.mX) * coeffX + gFigures[i].TopLeftBorder.mX;
                  coeffY := (abs(gFigures[i].BottomRightBorder.mY - mDoublePoints[0].mY + mDoublePoints[1].mY) - gFigures[i].TopLeftBorder.mY) / oldY;
                  gFigures[i].mDoublePoints[j].mY := (gFigures[i].mDoublePoints[j].mY - gFigures[i].TopLeftBorder.mY) * coeffY + gFigures[i].TopLeftBorder.mY;
              end;
            mDoublePoints[0] := mDoublePoints[1];
            end;
          1:
            begin
              gFigures[i].mDoublePoints[0].mX := gFigures[i].mDoublePoints[0].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              gFigures[i].mDoublePoints[0].mY := gFigures[i].mDoublePoints[0].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          2:
            begin
              gFigures[i].mDoublePoints[1].mX := gFigures[i].mDoublePoints[1].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              gFigures[i].mDoublePoints[0].mY := gFigures[i].mDoublePoints[0].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          3:
            begin
              gFigures[i].mDoublePoints[1].mX := gFigures[i].mDoublePoints[1].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              gFigures[i].mDoublePoints[1].mY := gFigures[i].mDoublePoints[1].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          4:
            begin
              gFigures[i].mDoublePoints[0].mX := gFigures[i].mDoublePoints[0].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          5:
            begin
              gFigures[i].mDoublePoints[0].mY := gFigures[i].mDoublePoints[0].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          6:
            begin
              gFigures[i].mDoublePoints[1].mX := gFigures[i].mDoublePoints[1].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
              mDoublePoints[0] := mDoublePoints[1];
            end;
          7:
            begin
              gFigures[i].mDoublePoints[1].mY := gFigures[i].mDoublePoints[1].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
              mDoublePoints[0] := mDoublePoints[1];
            end;
        end;
      end;
    end;
    if gFigures[i].mIsMoving then
    begin
      for j := 0 to high(gFigures[i].mDoublePoints) do
      begin
        gFigures[i].mDoublePoints[j].mX := gFigures[i].mDoublePoints[j].mX - mDoublePoints[0].mX + mDoublePoints[1].mX;
        gFigures[i].mDoublePoints[j].mY := gFigures[i].mDoublePoints[j].mY - mDoublePoints[0].mY + mDoublePoints[1].mY;
      end;
      mDoublePoints[0] := mDoublePoints[1];
    end;
  end;
end;

procedure TEditing.MouseUp(x, y: integer; Shift: TShiftState; Panel: TPanel);
var
  Figure: TFigure;
begin
  for Figure in gFigures do
  begin
   Figure.mIsEdited:=false;
   Figure.mIsMoving:=false;
   mIndex := -1;
  end;
end;

procedure TEditing.MouseDown(x, y: Integer);
var
  dp: TDoublePoint;
  Figure: TFigure;
  x1, y1, x2, y2, i: Integer;
begin
  dp := CanvasToWorld(x, y);
  for Figure in gFigures do
  begin
    for i := 0 to high(Figure.mAnchors) do
    begin
     x1 := Figure.mAnchors[i].x1;
     y1 := Figure.mAnchors[i].y1;
     x2 := Figure.mAnchors[i].x2;
     y2 := Figure.mAnchors[i].y2;
     if (x>=x1) and (x <= x2) and (y>=y1) and (y <= y2) then
     begin
       Figure.mIsEdited := true;
       mIndex := Figure.mAnchors[i].index;
       Figure.setPoints();
       break;
     end;
    end;
    if (Figure.IsPointInhere(dp)) and (Figure.mIsSelected) then //Сделать попадание по рамке вместо попадания по фигуре
      Figure.mIsMoving := true;
  end;
end;

initialization

  registerTools([THand, TLoupe, TSelection, TEditing]);
end.






