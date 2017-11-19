unit ufigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Spin, StdCtrls, Types, Math, uCoordinates, uProperty;

type

  TFigure = class
  private
    mDoublePoints: array of TDoublePoint;
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
    property TopLeftBorder: TDoublePoint read GetTopLeft;
    property BottomRightBorder: TDoublePoint read GetBottomRight;
    constructor Create(x, y: Double; Button: TMouseButton);
    procedure Paint(Canvas: TCanvas); virtual;
    procedure Update(x, y: Integer); virtual; abstract;
    class procedure SetParameters(panel: TPanel); virtual; abstract;
  end;

  TFigureClass = class of TFigure;

  TPolyline = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    class procedure SetParameters(panel: TPanel); override;
  end;

  TLine = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
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
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    class procedure SetParameters(panel: TPanel); override;
  end;

  TRoundRectangle = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    class procedure SetParameters(panel: TPanel); override;
  end;

  TEllipse = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    class procedure SetParameters(panel: TPanel); override;
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
  mPenColor := TPenColor.sPenColor;
  mPenStyle := TPenStyle.sPenStyle;
  mPenWidth := TPenWidth.sPenWidth;
  mBrushColor:= TBrushColor.sBrushColor;
  mBrushStyle := TBrushStyle.sBrushStyle;
  mRX := TRoundRect.sRX;
  mRY := TRoundRect.sRY;
  mButton := Button;
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

initialization

registerFigures([TPolyline, TLine, TRectangle,
                 TRoundRectangle, TEllipse]);
end.

