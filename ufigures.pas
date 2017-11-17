unit ufigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Spin, StdCtrls, Types, Math, uCoordinates;

type

  TPenStyleItem = record
    Name: String;
    PenStyle: TPenStyle
  end;

  TBrushStyleItem = record
    Name: String;
    BrushStyle: TBrushStyle;
  end;

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
    const
      PEN_STYLES: array[0..5] of TPenStyleItem =
        (
          (Name: 'Solid';         PenStyle: psSolid),
          (Name: 'No line';       PenStyle: psClear),
          (Name: 'Dots';          PenStyle: psDot),
          (Name: 'Dashes';        PenStyle: psDash),
          (Name: 'Dash dots';     PenStyle: psDashDot),
          (Name: 'Dash dot dots'; PenStyle: psDashDotDot)
        );
      BRUSH_STYLES: array[0..7] of TBrushStyleItem =
        (
          (Name: 'Solid';              BrushStyle: bsSolid),
          (Name: 'Hollow';             BrushStyle: bsClear),
          (Name: 'Horizontal stripes'; BrushStyle: bsHorizontal),
          (Name: 'Vertical stripes';   BrushStyle: bsVertical),
          (Name: 'Left diagonal';      BrushStyle: bsFDiagonal),
          (Name: 'Right diagonal';     BrushStyle: bsBDiagonal),
          (Name: 'Cross';              BrushStyle: bsCross),
          (Name: 'Diagonal cross';     BrushStyle: bsDiagCross)
        );
  protected
    function GetTopLeft: TDoublePoint;
    function GetBottomRight: TDoublePoint;
    class procedure CreateColorButton(panel: TPanel; Name: string;
                                      Color: TColor; handler: TNotifyEvent);
    class procedure CreateSpinEdit(panel: TPanel; Name: String;
                                  Width: Integer; handler: TNotifyEvent);
    class procedure CreatePenStyleComboBox(panel: TPanel; Name: String; penStyle: Integer; handler: TNotifyEvent);
    class procedure CreateBrushStyleComboBox(panel: TPanel; Name: String; brushStyle: Integer; handler: TNotifyEvent);
    class procedure PenColorChange(Sender: TObject);
    class procedure BrushColorChange(Sender: TObject);
    Class procedure PenWidthChange(Sender: TObject);
    class procedure PenStyleChange(Sender: TObject);
    class procedure BrushStyleChange(Sender: TObject);
    class procedure ChangeRX(Sender: TObject);
    Class procedure ChangeRY(Sender: TObject);
  public
    sPenColor: TColor; static;
    sBrushColor: TColor; static;
    sPenWidth: Integer; static;
    sPenStyle: Integer; static;
    sBrushStyle: Integer; static;
    sRX, sRY: Integer; static;
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
  mPenColor := sPenColor;
  mPenStyle := sPenStyle;
  mPenWidth := sPenWidth;
  mBrushColor:= sBrushColor;
  mBrushStyle := sBrushStyle;
  mRX := sRX;
  mRY := sRY;
  mButton := Button;
end;

procedure TFigure.Paint(Canvas: TCAnvas);
begin
  with Canvas do
  begin
    Pen.Color := mPenColor;
    Pen.Width := mPenWidth;
    Brush.Color := mBrushColor;
    Pen.Style := PEN_STYLES[mPenStyle].PenStyle;
    Brush.Style:= BRUSH_STYLES[mBrushStyle].BrushStyle;
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

{Buttons}
class procedure TFigure.CreateColorButton(panel: TPanel; Name: string;
                                          Color: TColor; handler: TNotifyEvent);
var
  PenColorBox: TColorButton;

begin
  PenColorBox := TColorButton.Create(panel);
  PenColorBox.Parent := panel;
  PenColorBox.Align := alTop;
  PenColorBox.Caption := Name;
  PenColorBox.ShowHint := true;
  PenColorBox.Hint := name;
  PenColorBox.Height := 40;
  PenColorBox.ButtonColor := Color;
  PenColorBox.OnColorChanged := handler;
end;

class procedure TFigure.CreateSpinEdit(panel: TPanel; Name: String;
                                      Width: Integer; handler: TNotifyEvent);
var
  Edit: TSpinEdit;
begin
  Edit := TSpinEdit.Create(panel);
  Edit.Parent := panel;
  Edit.Align:= alTop;
  Edit.ShowHint := true;
  Edit.Hint := name;
  Edit.AutoSelect := false;
  Edit.Value:= width;
  Edit.OnChange := handler;
end;

class procedure TFigure.CreatePenStyleComboBox(panel: TPanel; Name: String; penStyle: Integer; handler: TNotifyEvent);
var
  ComboBox: TComboBox;
  i: Integer;
begin
  ComboBox := TComboBox.Create(panel);
  ComboBox.Parent := panel;
  ComboBox.Align:= alTop;
  ComboBox.ShowHint := true;
  ComboBox.Hint := name;
  ComboBox.ReadOnly := true;
  for i := 0 to high(PEN_STYLES) do
    comboBox.Items.Add(PEN_STYLES[i].Name);
  ComboBox.ItemIndex := penStyle;
  ComboBox.OnChange := handler;
end;

class procedure TFigure.CreateBrushStyleComboBox(panel: TPanel; Name: String; brushStyle: Integer; handler: TNotifyEvent);
var
  ComboBox: TComboBox;
  i: Integer;
begin
  ComboBox := TComboBox.Create(panel);
  ComboBox.Parent := panel;
  ComboBox.Align:= alTop;
  ComboBox.ShowHint:= true;
  ComboBox.Hint:= name;
  ComboBox.ReadOnly := true;
  for i := 0 to high(BRUSH_STYLES) do
    comboBox.Items.Add(BRUSH_STYLES[i].Name);
  ComboBox.ItemIndex := brushStyle;
  ComboBox.OnChange := handler;
end;

{Button's handlers}
class procedure TFigure.PenStyleChange(Sender: TObject);
begin
  sPenStyle := (Sender as TComboBox).ItemIndex;
end;

class procedure TFigure.BrushStyleChange(Sender: TObject);
begin
  sBrushStyle :=  (Sender as TComboBox).ItemIndex;
end;

class procedure TFigure.BrushColorChange(Sender: TObject);
begin
  sBrushColor := (Sender as TColorButton).ButtonColor;
end;

class procedure TFigure.PenColorChange(Sender: TObject);
begin
  sPenColor := (Sender as TColorButton).ButtonColor;
end;

Class procedure TFigure.PenWidthChange(Sender: TObject);
begin
  sPenWidth := (Sender as TSpinEdit).Value;
end;

class procedure TFigure.ChangeRX(Sender: TObject);
begin
  sRX := (Sender as TSpinEdit).Value;
end;

Class procedure TFigure.ChangeRY(Sender: TObject);
begin
  sRY := (Sender as TSpinEdit).Value;
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
  CreateColorButton(Panel, 'Pen color', sPenColor, @PenColorChange);
  CreateSpinEdit(Panel, 'Pen width', sPenWidth, @PenWidthChange);
  CreatePenStyleComboBox(panel, 'Pen style', sPenStyle, @PenStyleChange);
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
  CreateColorButton(Panel, 'Line color', sPenColor, @PenColorChange);
  CreateSpinEdit(Panel, 'Line width', sPenWidth, @PenWidthChange);
  CreatePenStyleComboBox(panel, 'Line style', sPenStyle, @PenStyleChange);
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
  CreateColorButton(Panel, 'Brush color', sBrushColor, @BrushColorChange);
  CreateColorButton(panel, 'Line color', sPenColor, @PenColorChange);
  CreateSpinEdit(Panel, 'Line width', sPenWidth, @PenWidthChange);
  CreatePenStyleComboBox(panel, 'Line style', sPenStyle, @PenStyleChange);
  CreateBrushStyleComboBox(panel, 'Brush style', sBrushStyle, @BrushStyleChange);
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
  CreateColorButton(Panel, 'Brush color', sBrushColor, @BrushColorChange);
  CreateColorButton(panel, 'Line color', sPenColor, @PenColorChange);
  CreateSpinEdit(Panel, 'Line width', sPenWidth, @PenWidthChange);
  CreateSpinEdit(panel, 'RX', sRX, @ChangeRX);
  CreateSpinEdit(panel, 'RY', sRY, @ChangeRY);
  CreatePenStyleComboBox(panel, 'Line style', sPenStyle, @PenStyleChange);
  CreateBrushStyleComboBox(panel, 'Brush style', sBrushStyle, @BrushStyleChange);
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
  CreateColorButton(Panel, 'Brush color', sBrushColor, @BrushColorChange);
  CreateColorButton(panel, 'Line color', sPenColor, @PenColorChange);
  CreateSpinEdit(Panel, 'Line width', sPenWidth, @PenColorChange);
  CreatePenStyleComboBox(panel, 'Line style', sPenStyle, @PenStyleChange);
  CreateBrushStyleComboBox(panel, 'Brush style', sBrushStyle, @BrushStyleChange);
end;

initialization

registerFigures([TPolyline, TLine, TRectangle,
                 TRoundRectangle, TEllipse]);
end.

