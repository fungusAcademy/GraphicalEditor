unit ufigures;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, Buttons, Types, Math, LCLIntf, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, typinfo, Clipbrd, uCoordinates, uProperty;

type
  TSavedState = procedure(IsSaved: Boolean) of Object;
  TChangeEvent = procedure of Object;

  { TFigure }

  TFigure = class
  type
    TAnchor = record
    x1, y1, x2, y2: Integer;
    index: Integer;
  end;
  private
    mButton: TMouseButton;
    fPenColor: TColor;
    fPenWidth: Integer;
    fPenStyle: Integer;
    fBrushColor: TColor;
    fBrushStyle: Integer;
    fRX, fRY: Integer;
  protected
    function GetTopLeft: TDoublePoint;
    function GetBottomRight: TDoublePoint;
  public
    mI: Integer;
    mIsDrawing: boolean;
    mIsSelected: boolean;
    mIsEdited: boolean;
    mIsMoving: boolean;
    mValidProperties: array [0..6] of boolean;
    mAnchors: array of TAnchor;
    mDoublePoints: array of TDoublePoint;
    ToSavedState: TSavedState; static;
    OnChange: TChangeEvent; static;
    property TopLeftBorder: TDoublePoint read GetTopLeft;
    property BottomRightBorder: TDoublePoint read GetBottomRight;
    procedure getParameters(); virtual; abstract;
    procedure Paint(Canvas: TCanvas); virtual;
    procedure Update(x, y: Integer); virtual;
    procedure MouseUp(x, y: Integer); virtual;
    procedure DrawFrame(Canvas: TCanvas); virtual;
    procedure DrawAnchors(Canvas: TCanvas); virtual;
    procedure SetPoints(); virtual;
    procedure setStyles(); virtual;
    procedure sendStyles(); virtual;
    procedure SetValuesOfFigures(ANode: TDOMNode); virtual;
    procedure LoadProps(ANode: TDOMNode); virtual;
    procedure addPoint(dp: TDoublePoint);
    procedure setPenColor(Color: TColor);
    procedure setBrushColor(color: TColor);
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; virtual;
    function IsPointInhere(dp: TDoublePoint): boolean; virtual; abstract;
    class procedure SetStyleButtons(panel: TPanel); virtual;
    class procedure LoadFigure(ANode: TDOMNode); virtual; abstract;
    class procedure SaveToFile(FileName: string);
    class procedure LoadPrev();
    class procedure LoadNext();
    class procedure InitHistory();
    class procedure SaveToHistory;
    class procedure copySelected;
    class procedure pasteSelected;
    class function GetLastFigure: TFigure;
    class function LoadFile(FileName: String): Boolean;
    constructor Create(x, y: Double; Button: TMouseButton); overload;
    constructor Create(); overload;
  published
    property PenWidth: Integer read fPenWidth write fPenWidth;
    property PenStyle: Integer read fPenStyle write fPenStyle;
    property BrushStyle: Integer read fBrushStyle write fBrushStyle;
    property RX: Integer read fRX write fRX;
    property RY: Integer read fRY write fRY;

  end;

  TFigureClass = class of TFigure;

  TPolyline = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: Integer); override;
    procedure DrawAnchors(Canvas: TCanvas); override;
    procedure SetPoints(); override;
    procedure getParameters(); override;
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    class procedure LoadFigure(ANode: TDOMNode); override;
  end;

  TLine = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure SetPoints(); override;
    procedure getParameters(); override;
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    class procedure LoadFigure(ANode: TDOMNode); override;
  end;

  TRectangle = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure getParameters(); override;
    procedure setStyles(); override;
    procedure sendStyles(); override;
    procedure SetValuesOfFigures(ANode: TDOMNode); override;
    procedure LoadProps(ANode: TDOMNode); override;
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    class procedure LoadFigure(ANode: TDOMNode); override;
    class procedure SetStyleButtons(panel: TPanel); override;
  end;


  TRoundRectangle = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure getParameters(); override;
    procedure setStyles(); override;
    procedure sendStyles(); override;
    procedure SetValuesOfFigures(ANode: TDOMNode); override;
    procedure LoadProps(ANode: TDOMNode); override;
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    class procedure LoadFigure(ANode: TDOMNode); override;
    class procedure SetStyleButtons(panel: TPanel); override;
  end;

  TEllipse = class(TFigure)
  public
    procedure Paint(Canvas: TCanvas); override;
    procedure getParameters(); override;
    procedure setStyles(); override;
    procedure sendStyles(); override;
    procedure SetValuesOfFigures(ANode: TDOMNode); override;
    procedure LoadProps(ANode: TDOMNode); override;
    function IsPointInhere(dp: TDoublePoint): boolean; override;
    class procedure SetStyleButtons(panel: TPanel); override;
    class procedure LoadFigure(ANode: TDOMNode); override;
  end;

var
  gFigures: array of TFigure;
  gFigureClasses: array of TFigureClass;
  Current, Saved: Integer;
  History: array of TStringStream;
  procedure SavedToCurrent();

implementation
{Save}
function FiguresToXML (num: Integer): TXMLDocument;
var
  Doc: TXMLDocument;
  FiguresNode: TDOMNode;
  i: Integer;
begin
  Doc:= TXMLDocument.Create;
  FiguresNode:= Doc.CreateElement('Figures');
  Doc.AppendChild(FiguresNode);
  FiguresNode:= Doc.DocumentElement;
  case num of
  0:
  for i:= 0 to High(gFigures) do
    FiguresNode.AppendChild(gFigures[i].SaveFigure(Doc));
  1:
  for i:= 0 to High(gFigures) do
    if gFIgures[i].mIsSelected then
      FiguresNode.AppendChild(gFigures[i].SaveFigure(Doc));
  end;
  Result:= Doc;
end;

class procedure TFigure.SaveToFile(FileName: string);
var
  Doc: TXMLDocument;
begin
  if (Copy(FileName, Length(FileName) - 3, 4) <> '.xml') then
    Exit;
  try
    Doc := FiguresToXML(0);
    WriteXML(Doc, FileName);
    Saved:= Current;
  finally
    Doc.Free;
  end;
end;

{Load}
function LoadFigures(Doc: TXMLDocument; num: Integer): Boolean;
var
  FigNode: TDOMNode;
  i: Integer;
begin
  Result:= True;
  if Doc.DocumentElement.NodeName <> 'Figures' then
      Exit(False);
  if num = 0 then
    SetLength(gFigures, 0);
  FigNode:= Doc.DocumentElement.FirstChild;
  while FigNode <> Nil do
  begin
    for i:=0 to High(gFigureClasses) do
      if FigNode.NodeName = gFigureClasses[i].ClassName then
        gFigureClasses[i].LoadFigure(FigNode);
    FigNode:= FigNode.GetNextNodeSkipChildren;
  end;
end;

class function TFigure.LoadFile(FileName: String): boolean;
var
  Doc: TXMLDocument;
begin
  if (Copy(FileName, Length(FileName) - 3, 4) <> '.xml') then
    Exit(False);
  try
    ReadXMLFile(Doc, FileName);
    Result := LoadFigures(Doc, 0);
  finally
    Doc.Free;
  end;
end;

procedure TFigure.LoadProps(ANode: TDOMNode);
begin
    try
      if ANode.Attributes.Length > 0 then
        SetPropValue(Self, 'PenWidth', ANode.Attributes.Item[0].NodeValue);
      if ANode.Attributes.Length > 1 then
        setPenColor(StrToInt(ANode.Attributes.Item[1].NodeValue));
      if ANode.Attributes.Length > 2 then
        SetPropValue(Self, 'PenStyle', ANode.Attributes.Item[2].NodeValue);
    except on EConvertError do exit;
    end;
end;

{history}
procedure SavedToCurrent();
begin
  saved := current;
end;

procedure Load();
var
  Doc: TXMLDocument;
begin
  if (Current = Saved) or (current = 0) then
    TFigure.ToSavedState(True)
  else
    TFigure.ToSavedState(False);
  History[Current].Position:= 0;
  ReadXMLFile(Doc, History[Current]);
  LoadFigures(Doc, 0);
end;

class procedure TFigure.LoadPrev;
begin
  if Current = 0 then
    Exit;
  Dec(Current);
  Load();
end;

class procedure TFigure.LoadNext;
begin
  if Current = High(History) then
    Exit;
  Inc(Current);
  Load();
end;


class procedure TFigure.InitHistory;
var
  Doc: TXMLDocument;
begin
  SetLength(History, 1);
  History[0]:= TStringStream.Create('');
  Doc:= FiguresToXML(0);
  WriteXMLFile(Doc, History[0]);
  Current:= 0;
  Saved:= 0;
end;

class procedure TFigure.SaveToHistory;
var
  Doc: TXMLDocument;
begin
  try
    Doc:= FiguresToXML(0);
    SetLength(History, Current + 2);
    History[High(History)]:= TStringStream.Create('');
    WriteXMLFile(Doc, History[High(History)]);
    Current:= High(History);
    if Current <= Saved then
      Saved:= -1;
  finally
    Doc.Free;
  end;
end;

{copy/paste}
class procedure TFigure.copySelected();
var
  Doc: TXMLDocument;
  str: TStringStream;
begin
  try
    str := TStringStream.Create('');
    Doc:= FiguresToXML(1);
    WriteXMLFile(doc, str);
    Clipboard.AsText := str.DataString;
  finally
    Doc.Free;
    str.Free;
  end;
end;

class procedure TFigure.pasteSelected();
var
  Doc: TXMLDocument;
  str: TStringStream;
begin
  try
    str := TStringStream.Create('');
    str.WriteString(Clipboard.AsText);
    str.Position := 0;
    ReadXMLFile(doc, str);
    LoadFigures(Doc, 1);
    SaveToHistory();
    ToSavedState(false);
  finally
    Doc.Free;
    str.Free;
  end;
end;

{TFigure}
constructor TFigure.Create(x, y: Double; Button: TMouseButton);
var
  i: Integer;
begin
  SetLength(mDoublePoints, 2);
  mDoublePoints[0] := DoubleToPoint(x, y);
  mDoublePoints[1] := mDoublePoints[0];
  mButton := Button;
  mI := 3;
  mIsDrawing := true;
  for i := 0 to high(gFigures)-1 do
  begin
    gFigures[i].mIsSelected := false;
    mIsEdited := false;
    mIsMoving := false;
  end;
  setStyles();
  getParameters();
  OnChange();
end;

constructor TFigure.Create();
begin
end;

class function TFigure.GetLastFigure(): TFigure;
begin
  if Length(gFigures) > 0 then
    Result:= gFigures[High(gFigures)]
  else
    Result:= Nil;
end;

procedure TFigure.Update(x, y: Integer);
begin
  if mIsDrawing then
    if mButton = mbLeft then
      mDoublePoints[high(mDoublePoints)] := CanvasToWorld(x, y);
end;

procedure TFigure.addPoint(dp: TDoublePoint);
begin
  if length(mDoublePoints) > 0 then
    SetLength(mDoublePoints, Length(mDoublePoints) + 1)
  else
    SetLength(mDoublePoints, 1);
  mDoublePoints[High(mDoublePoints)]:= dp;
end;

procedure TFigure.Paint(Canvas: TCanvas);
begin
  if mIsSelected then
  begin
    //setStyles();
    DrawAnchors(canvas);
    DrawFrame(canvas);
  end;
  with Canvas do
  begin
    Pen.Color := fPenColor;
    Pen.Width := fPenWidth;
    Brush.Color := fBrushColor;
    Pen.Style := TPenStyle.PEN_STYLES[fPenStyle].PenStyle;
    Brush.Style:= TBrushStyle.BRUSH_STYLES[fBrushStyle].BrushStyle;
    RX := fRX;
    RY := fRY;
  end;
end;

procedure TFigure.MouseUp(x, y: Integer);
begin
  mIsDrawing := false;
  SaveToHistory;
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

procedure TFigure.DrawFrame(Canvas: TCanvas);
var
  p1, p2: TPoint;
begin
  with canvas do
  begin
    Pen.Color := clBlue;
    Pen.Width := 1;
    Brush.Style := bsClear;
    case mI of
      0: Pen.Style := psDash;
      1: Pen.Style := psDashDot;
      2: Pen.Style := psSolid;
    end;
  end;
  p1 := WorldToCanvas(TopLeftBorder.mX, TopLeftBorder.mY);
  p2 := WorldToCanvas(BottomRightBorder.mX, BottomRightBorder.mY);
  Canvas.Rectangle(p1.x - (PenWidth div 2) - 5, p1.y - (PenWidth div 2) - 5,
                  p2.x + (PenWidth div 2) + 5, p2.y + (PenWidth div 2) + 5);
end;

procedure TFigure.DrawAnchors(Canvas: TCanvas);
var
  x, y, w, i: Integer;
  ap: array[0..7] of TDoublePoint;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  setLength(mAnchors, 7);
  w := PenWidth div 2 + 5;

  ap[0].mX := TopLeftBorder.mX;
  ap[0].mY := BottomRightBorder.mY;
  ap[1] := TopLeftBorder;
  ap[2].mX := BottomRightBorder.mX;
  ap[2].mY := TopLeftBorder.mY;
  ap[3] := BottomRightBorder;
  ap[4].mX := TopLeftBorder.mX;
  ap[4].mY := (TopLeftBorder.mY + BottomRightBorder.mY) / 2;
  ap[5].mX := (TopLeftBorder.mX + BottomRightBorder.mX) / 2;
  ap[5].mY := TopLeftBorder.mY;
  ap[6].mX := BottomRightBorder.mX;
  ap[6].mY := (TopLeftBorder.mY + BottomRightBorder.mY) / 2;
  ap[7].mX := (TopLeftBorder.mX + BottomRightBorder.mX) / 2;
  ap[7].mY := BottomRightBorder.mY;

  for i := 0 to high(ap) do
  begin
    x := WorldToCanvas(ap[i]).x;
    y := WorldToCanvas(ap[i]).y;
    Canvas.Rectangle(x-w, y-w, x+w, y+w);
    mAnchors[i].x1 := x-w;
    mAnchors[i].y1 := y-w;
    mAnchors[i].x2 := x+w;
    mAnchors[i].y2 := y+w;
    mAnchors[i].index := i;
  end;
end;

procedure TFigure.SetPoints();
var
  dp: TDoublePoint;
begin
  if (mDoublePoints[0].mX > TopLeftBorder.mX) and (mDoublePoints[0].mY > TopLeftBorder.mY) then
  begin
    dp := mDoublePoints[0];
    mDoublePoints[0] := mDoublePoints[1];
    mDoublePoints[1] := dp;
  end
  else if (mDoublePoints[0].mX > TopLeftBorder.mX) then
  begin
    dp := mDoublePoints[0];
    mDoublePoints[0].mX := mDoublePoints[1].mX;
    mDoublePoints[1].mX := dp.mX;
  end
  else if (mDoublePoints[0].mY > TopLeftBorder.mY) then
  begin
    dp := mDoublePoints[0];
    mDoublePoints[0].mY := mDoublePoints[1].mY;
    mDoublePoints[1].mY := dp.mY;
  end;
end;

procedure TFigure.setStyles();
begin
  fPenColor := TPenColor.sPenColor;
  fPenStyle := TPenStyle.sPenStyle;
  fPenWidth := TPenWidth.sPenWidth;
end;

procedure TFigure.sendStyles();
begin
  TPenColor.sPenColor := fPenColor;
  TPenStyle.sPenStyle := PenStyle;
  TPenWidth.sPenWidth := PenWidth;
end;

procedure TFigure.setPenColor(Color: TColor);
begin
  fPenColor := color;
end;

procedure TFigure.setBrushColor(Color: TColor);
begin
  fBrushColor := color;
end;

procedure TFigure.SetValuesOfFigures(ANode: TDOMNode);
begin
    TDOMElement(ANode).SetAttribute('PenWidth', IntToStr(PenWidth));
    TDOMElement(ANode).SetAttribute('PenColor', IntToStr(fPenColor));
    TDOMElement(ANode).SetAttribute('PenStyle', IntToStr(PenStyle));
end;

function TFIgure.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: Integer;
begin
  Result:= ADoc.CreateElement(ClassName);
  Self.SetValuesOfFigures(Result);
  for i:= 0 to High(mDoublePoints) do
  begin
    PNode:= ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(mDoublePoints[i].mX));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(mDoublePoints[i].mY));
    Result.AppendChild(PNode);
  end;
end;

class procedure TFigure.SetStyleButtons(panel: TPanel);
begin
  TPenColor.CreatePenColorButton(Panel);
  TPenWidth.CreateWidthSpinEdit(Panel);
  TPenStyle.CreatePenStyleComboBox(panel);
end;
{Register figures}
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
  if mIsDrawing then
  begin
    if mButton = mbLeft then
      SetLength(mDoublePoints, length(mDoublePoints) + 1);
    inherited Update(x, y);
  end;
end;

function TPolyline.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y, x1, x2, y1, y2, c: Double;
  i, j, w: Integer;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  w := PenWidth + 5;
  for i := 0 to High(mDoublePoints) do
  begin
    // Ошибочка
    x1 := Min(mDoublePoints[i].mX, mDoublePoints[i + 1].mX);
    x2 := Max(mDoublePoints[i].mX, mDoublePoints[i + 1].mX);
    y1 := Min(mDoublePoints[i].mY, mDoublePoints[i + 1].mY);
    y2 := Max(mDoublePoints[i].mY, mDoublePoints[i + 1].mY);
    if (x1 = x2) then
    begin
      if (y >= y1) and (y <= y2) and (x <= x1 + w) and (x >= x1 - w) then
        Result := True
      else
        Result := False
    end
    else
      for j := 1 to 2 do
        if (sqr(x - x1) + sqr(y - y1) <= sqr(w / 2 + 2)) or
          (sqr(x - x2) + sqr(y - y2) <= sqr(w / 2 + 2)) or
          ((y >= (y2 - y1) / (x2 - x1) * x + y2 - (y2 - y1) / (x2 - x1) * x2 - w) and
          (y <= (y2 - y1) / (x2 - x1) * x + y2 - (y2 - y1) / (x2 - x1) * x2 + w) and
          (x >= x1) and (x <= x2)) then
        begin
          Result := True;
          exit;
        end
        else
        begin
          c := y2;
          y2 := y1;
          y1 := c;
          Result := False;
        end;
  end;
end;

procedure TPolyline.DrawAnchors(Canvas: TCanvas);
var
  x, y, i, w: Integer;
begin
  //inherited DrawAnchors(canvas);
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Width := 2;
  Canvas.Brush.Style := bsClear;
  setLength(mAnchors, high(mDoublePoints));
  w := PenWidth div 2 + 5;
  for i := 0 to high(mDoublePoints) do
  begin
    x := WorldToCanvas(mDOublePoints[i]).x;
    y := WorldToCanvas(mDOublePoints[i]).y;
    Canvas.Rectangle(x-w, y-w, x+w, y+w);
    mAnchors[i].x1 := x-w;
    mAnchors[i].y1 := y-w;
    mAnchors[i].x2 := x+w;
    mAnchors[i].y2 := y+w;
    mAnchors[i].Index := i;
  end;
end;

procedure TPolyline.SetPoints();
begin
  //
end;

procedure TPolyline.getParameters();
var
  i: Integer;
begin
  for i := 0 to 2 do
    mValidProperties[i] := true;
end;

class procedure TPolyline.LoadFigure(ANode: TDOMNode);
var
  polyline: TPolyLine;
  i: Integer;
  PNode: TDOMNode;
begin
  SetLength(gFigures, Length(gFigures) + 1);
  polyline:= TPolyLine.Create();
  polyline.LoadProps(ANode);
  PNode:= ANode;
    for i:= 1 to ANode.GetChildCount do
    begin
      try
        PNode:= PNode.GetNextNode;
        Polyline.AddPoint(DoubleToPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
                                        StrToFloat(PNode.Attributes.Item[1].NodeValue)));
      except on Exception do exit;
      end;
    end;
  gFigures[high(gFigures)] := polyline;
end;

{Line}
procedure TLine.Paint(Canvas: TCanvas);
begin
  inherited Paint(Canvas);
  Canvas.Line(WorldToCanvas(mDoublePoints[0]), WOrldTOCanvas(mDoublePoints[1]));
end;

procedure TLine.getParameters();
var
  i: Integer;
begin
  for i := 0 to 2 do
    mValidProperties[i] := true;
end;

function TLine.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
  c: Double;
  i, w: Integer;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := Min(mDoublePoints[0].mX, mDoublePoints[1].mX);
  x2 := max(mDoublePoints[0].mX, mDoublePoints[1].mX);
  y1 := min(mDoublePoints[0].mY, mDoublePoints[1].mY);
  y2 := max(mDoublePoints[0].mY, mDoublePoints[1].mY);
  w := PenWidth + 2;
  if (x1 = x2) then
    if (y >= y1) and (y <= y2) and (x <= x1 + w) and (x >= x1 - w) then
      Result := True
    else
      Result := False
  else
   begin
      for i := 0 to 1 do
      if (sqr(x - x1) + sqr(y - y1) <= sqr(w / 2 + 2)) or
         (sqr(x - x2) + sqr(y - y2) <= sqr(w / 2 + 2)) or
         ((y >= (y2 - y1) / (x2 - x1) * x + y2 - (y2 - y1) / (x2 - x1) * x2 - w) and
         (y <= (y2 - y1) / (x2 - x1) * x + y2 - (y2 - y1) / (x2 - x1) * x2 + w) and
         (x >= x1) and (x <= x2)) then
      begin
        Result := True;
        break;
      end
      else
      begin
        c := y2;
        y2 := y1;
        y1 := c;
        Result := False;
      end;
   end
end;

procedure TLine.SetPoints();
begin
  inherited SetPoints();
end;

class procedure TLine.LoadFigure(ANode: TDOMNode);
var
  Line: TLine;
  i: Integer;
begin
  SetLength(gFigures, Length(gFigures) + 1);
  Line:= TLine.Create;
  Line.LoadProps(ANode);
  for i := 0 to 1 do
  begin
    try
      ANode:= ANode.GetNextNode;
      Line.addPoint(DoubleToPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                                  StrToFloat(ANode.Attributes.Item[1].NodeValue)));
    except on Exception do exit;
           //on EDOMError do exit;
    end;
  end;
  gFigures[high(gFigures)] := line;
end;

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

class procedure TRectangle.SetStyleButtons(panel: TPanel);
begin
  inherited SetStyleButtons(panel);
  TBrushColor.CreateBrushColorButton(panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
end;

procedure TRectangle.getParameters();
var
  i: Integer;
begin
  for i := 0 to 4 do
    mValidProperties[i] := true;
end;

function TRectangle.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := TopLeftBorder.mX;
  y1 := TopLeftBorder.mY;
  x2 := BottomRightBorder.mX;
  y2 := BottomRightBorder.mY;
  if (x <= x2) and (x >= x1) and (y <= y2) and (y >= y1) then
      Result := true;
end;

procedure TRectangle.setStyles();
begin
  inherited setStyles();
  fBrushColor:= TBrushColor.sBrushColor;
  fBrushStyle := TBrushStyle.sBrushStyle;
end;

procedure TRectangle.sendStyles();
begin
  inherited sendStyles;
  TBrushColor.sBrushColor := fBrushColor;
  TBrushStyle.sBrushStyle := BrushStyle;
end;

procedure TRectangle.SetValuesOfFigures(ANode: TDOMNode);
begin
  inherited SetValuesOfFigures(ANode);
  TDOMElement(ANode).SetAttribute('BrushColor', IntToStr(fBrushColor));
  TDOMElement(ANode).SetAttribute('BrushStyle', IntToStr(BrushStyle));
end;

procedure TRectangle.LoadProps(ANode: TDOMNode);
begin
  inherited LoadProps(ANode);
  try
    if ANode.Attributes.Length > 3 then
      setBrushColor(strToInt(ANode.Attributes.Item[3].NodeValue));
    if ANode.Attributes.Length > 4 then
      SetPropValue(Self, 'BrushStyle', ANode.Attributes.Item[4].NodeValue);
  except on EConvertError do exit;
  end;
end;

class procedure TRectangle.LoadFigure(ANode: TDOMNode);
var
  Rect: TRectangle;
  i: Integer;
begin
  SetLength(gFigures, Length(gFigures) + 1);
  Rect:= TRectangle.Create;
  Rect.LoadProps(ANode);
  for i := 0 to 1 do
  begin
    try
      ANode:= ANode.GetNextNode;
      Rect.addPoint(DoubleToPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                                  StrToFloat(ANode.Attributes.Item[1].NodeValue)));
    except on Exception do exit;
    end;
  end;
  gFigures[High(gFigures)]:= Rect;
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
                  CanvasBottomRight.x, CanvasBottomRight.y, RX, RY);
end;

class procedure TRoundRectangle.SetStyleButtons(panel: TPanel);
begin
  inherited SetStyleButtons(panel);
  TBrushColor.CreateBrushColorButton(Panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
  TRoundRect.CreateRXSpinEdit(panel);
  TRoundRect.CreateRYSpinEdit(panel);
end;

procedure TRoundRectangle.getParameters();
var
  i: integer;
begin
  for i := 0 to 6 do
    mValidProperties[i] := true
end;

function TRoundRectangle.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
  round: Integer;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := TopLeftBorder.mX;
  y1 := TopLeftBorder.mY;
  x2 := BottomRightBorder.mX;
  y2 := BottomRightBorder.mY;
  round := (RX + RY) div 2;
  if ((x >= x1) and (x <= x2) and (y >= y1 + round) and (y <= y2 - round)) or
      ((x >= x1 + round) and (x <= x2 - round) and (y >= y1) and (y <= y2)) or
      (sqr(x - x1 - round) + sqr(y - y1 - round) <= sqr(round)) or
      (sqr(x - x2 + round) + sqr(y - y1 - round) <= sqr(round)) or
      (sqr(x - x1 - round) + sqr(y - y2 + round) <= sqr(round)) or
      (sqr(x - x2 + round) + sqr(y - y2 + round) <= sqr(round))
      then Result := true;
end;

procedure TRoundRectangle.setStyles();
begin
  inherited setStyles();
  fBrushColor:= TBrushColor.sBrushColor;
  fBrushStyle := TBrushStyle.sBrushStyle;
  fRX := TRoundRect.sRX;
  fRY := TRoundRect.sRY;
end;

procedure TRoundRectangle.sendStyles();
begin
  inherited sendStyles;
  TBrushColor.sBrushColor := fBrushColor;
  TBrushStyle.sBrushStyle := BrushStyle;
  TRoundRect.sRX := RX;
  TRoundRect.sRY := RY;
end;

procedure TRoundRectangle.SetValuesOfFigures(ANode: TDOMNode);
begin
  inherited SetValuesOfFigures(ANode);
  TDOMElement(ANode).SetAttribute('BrushColor', IntToStr(fBrushColor));
  TDOMElement(ANode).SetAttribute('BrushStyle', IntToStr(BrushStyle));
  TDOMElement(ANode).SetAttribute('RX', IntToStr(RX));
  TDOMElement(ANode).SetAttribute('RY', IntToStr(RY));
end;

procedure TRoundRectangle.LoadProps(ANode: TDOMNode);
begin
  inherited LoadProps(ANode);
  try
     if ANode.Attributes.Length > 3 then
      setBrushColor(StrToInt(ANode.Attributes.Item[3].NodeValue));
     if ANode.Attributes.Length > 4 then
      SetPropValue(Self, 'BrushStyle', ANode.Attributes.Item[4].NodeValue);
     if ANode.Attributes.Length > 5 then
      SetPropValue(Self, 'RX', ANode.Attributes.Item[5].NodeValue);
     if ANode.Attributes.Length > 6 then
      SetPropValue(Self, 'RY', ANode.Attributes.Item[6].NodeValue);
  except on EConvertError do exit;
  end;
end;

class procedure TRoundRectangle.LoadFigure(ANode: TDOMNode);
var
  RoundRect: TRoundRectangle;
  i: Integer;
begin
  SetLength(gFigures, Length(gFigures) + 1);
  RoundRect:= TRoundRectangle.Create;
  RoundRect.LoadProps(ANode);
  for i := 0 to 1 do
  begin
    try
      ANode:= ANode.GetNextNode;
      RoundRect.addPoint(DoubleToPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                                       StrToFloat(ANode.Attributes.Item[1].NodeValue)));
    except on Exception do exit;
    end;
  end;
  gFigures[High(gFigures)]:= RoundRect;
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

class procedure TEllipse.SetStyleButtons(panel: TPanel);
begin
  inherited SetStyleButtons(panel);
  TBrushColor.CreateBrushColorButton(Panel);
  TBrushStyle.CreateBrushStyleComboBox(panel);
end;

procedure TEllipse.getParameters();
var
  i: integer;
begin
  for i := 0 to 4 do
    mValidProperties[i] := true
end;

function TEllipse.IsPointInhere(dp: TDoublePoint): boolean;
var
  x, y: double;
  x1, x2: double;
  y1, y2: double;
begin
  Result := false;
  x := dp.mX;
  y := dp.mY;
  x1 := TopLeftBorder.mX; //Зависимость о ширины
  y1 := TopLeftBorder.mY;
  x2 := BottomRightBorder.mX;
  y2 := BottomRightBorder.mY;
  if ((x1 - x2) <> 0) and ((y1 - y2) <> 0) then
  begin
   if (sqr(x - ((x1 + x2) / 2)) / sqr((x1 - x2) / 2) +
       sqr(y - ((y1 + y2) / 2)) / sqr((y1 - y2) / 2)) <= 1 then
       Result := true;
  end;
end;

procedure TEllipse.setStyles();
begin
  inherited setStyles();
  fBrushColor:= TBrushColor.sBrushColor;
  fBrushStyle := TBrushStyle.sBrushStyle;
end;

procedure TEllipse.sendStyles();
begin
  inherited sendStyles;
  TBrushColor.sBrushColor := fBrushColor;
  TBrushStyle.sBrushStyle := BrushStyle;
end;

procedure TEllipse.SetValuesOfFigures(ANode: TDOMNode);
begin
  inherited SetValuesOfFigures(ANode);
  TDOMElement(ANode).SetAttribute('BrushColor', IntToStr(fBrushColor));
  TDOMElement(ANode).SetAttribute('BrushStyle', IntToStr(BrushStyle));
end;

procedure TEllipse.LoadProps(ANode: TDOMNode);
begin
  inherited LoadProps(ANode);
    try
      if ANode.Attributes.Length > 3 then
        setBrushColor(StrToInt(ANode.Attributes.Item[3].NodeValue));
      if ANode.Attributes.Length > 4 then
        SetPropValue(Self, 'BrushStyle', ANode.Attributes.Item[4].NodeValue);
    except on EConvertError do exit;
    end;
end;

class procedure TEllipse.LoadFigure(ANode: TDOMNode);
var
  Ellipse: TEllipse;
  i: Integer;
begin
  SetLength(gFigures, Length(gFigures) + 1);
  Ellipse:= TEllipse.Create;
  Ellipse.LoadProps(ANode);
  for i := 0 to 1 do
  begin
    try
      ANode:= ANode.GetNextNode;
      if ANode.Attributes.Length < 2 then exit;
      Ellipse.addPoint(DoubleToPoint(StrToFloat(ANode.Attributes.Item[0].NodeValue),
                                     StrToFloat(ANode.Attributes.Item[1].NodeValue)));
    except on Exception do exit;
    end;

  end;
  gFigures[High(gFigures)]:= Ellipse;
end;

initialization

registerFigures([TPolyline, TLine, TRectangle,
                 TRoundRectangle, TEllipse]);
end.

