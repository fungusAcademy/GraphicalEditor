unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Buttons, StdCtrls, ColorBox, ufigures, uTools, uCoordinates, uProperty, Types;

type

  { TMainForm }

  TAction = (ACTION_FIGURE, ACTION_TOOL);

  TMainForm = class(TForm)
  private
    mIsDrawing: boolean;
    mCurrentFigure: TFigureClass;
    mCurrentTool: TToolClass;
    mCurrentAction: TAction;
    mFigures: array of TFigure;
    mTools: array of TTool;
    const
      BTN_SIZE = 40;
      BTN_MARGIN = 8;
      BTN_PADDING = 1;
      START_PEN_COLOR: TColor = clBlack;
      START_BRUSH_COLOR: TColor = clWhite;
      START_PEN_STYLE: integer = 0;
      START_BRUSH_STYLE: integer = 1;
      START_PEN_WIDTH: integer = 1;
      CANVAS_OFFSET_BORDER_SIZE = 10;
      START_RX = 30;
      START_RY = 30;
  published
    HorScrollBar: TScrollBar;
    VerScrollBar: TScrollBar;
    MainMenu: TMainMenu;
    MenuItemSetDefault: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemClearAll: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemInfo: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PaintBox: TPaintBox;
    ToolsPanel: TPanel;
    StylePanel: TPanel;
    procedure FormResize(Sender: TObject);
    procedure MenuItemSetDefaultClick(Sender: TObject);
    procedure ClearAllMenuItemClick(Sender: TObject);
    procedure createForm(Sender: TObject);
    procedure closeForm(Sender: TObject; var CloseAction: TCloseAction);

    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
                                Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
                              Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
                                WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ToolBtnClick(Sender: TObject);
    procedure FigureBtnClick(Sender: TObject);
    procedure SetStylePanel();
    procedure SetScrollBars();
    procedure Scroll(Sender: TObject;
                    ScrollCode: TScrollCode; var ScrollPos: Integer);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm constructor }
procedure TMainForm.createForm(Sender: TObject);
var
  Btn: TSpeedButton;
  i: integer;
  CurrentIcon: TPicture;
  iconsPerRow: integer;

begin
  MainForm.DoubleBuffered := True;
  MainForm.Caption := ApplicationName;
  mIsDrawing := False;

  TPenColor.sPenColor := START_PEN_COLOR;
  TPenWidth.sPenWidth := START_PEN_WIDTH;
  TBrushColor.sBrushColor := START_BRUSH_COLOR;
  TPenStyle.sPenStyle := START_PEN_STYLE;
  TBrushStyle.sBrushStyle := START_BRUSH_STYLE;
  TRoundRect.sRX := START_RX;
  TRoundRect.sRY := START_RY;

  iconsPerRow := ToolsPanel.Width div (BTN_SIZE + BTN_MARGIN + BTN_PADDING);
  ToolsPanel.Height := ((Length(gFigureClasses) + length(gToolClasses)) div iconsPerRow) *
                        (BTN_SIZE + BTN_MARGIN) + BTN_PADDING * 2;

  for i := low(gFigureClasses) to high(gFigureClasses) do
  begin
    Btn := TSpeedButton.Create(ToolsPanel);
    Btn.Parent := ToolsPanel;
    Btn.Name := gFigureClasses[i].ClassName + 'Button';
    Btn.GroupIndex := 1;
    Btn.Tag := i;
    Btn.OnClick := @FigureBtnClick;

    CurrentIcon := TPicture.Create;
    CurrentIcon.LoadFromFile('icons/' + gFigureClasses[i].ClassName + '.png');
    Btn.Glyph := CurrentIcon.Bitmap;
    CurrentIcon.Free;

    Btn.Left := (i mod iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Top  := (i div iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Width := BTN_SIZE + BTN_MARGIN;
    Btn.Height := Btn.Width;
  end;

  for i := low(gToolClasses) to high(gToolClasses) do
  begin
    Btn := TSpeedButton.Create(ToolsPanel);
    Btn.Parent := ToolsPanel;
    Btn.Name := gToolClasses[i].ClassName + 'Tool';
    Btn.GroupIndex := 1;
    Btn.Tag := i;
    Btn.OnClick := @ToolBtnClick;

    CurrentIcon := TPicture.Create;
    CurrentIcon.LoadFromFile('icons/' + gToolClasses[i].ClassName + '.png');
    Btn.Glyph := CurrentIcon.Bitmap;
    CurrentIcon.Free;

    Btn.Left := ((Length(gFigureClasses) + i) mod iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Top  := ((Length(gFigureClasses) + i) div iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Width := BTN_SIZE + BTN_MARGIN;
    Btn.Height := Btn.Width;
  end;

  TSpeedButton(ToolsPanel.Controls[0]).Click;
  TSpeedButton(ToolsPanel.Controls[0]).Down:= true;

  gCanvasWidth := PaintBox.Width;
  gCanvasHeight := PaintBox.Height;

  SetScrollBars;
end;

{Style panel constructor}
procedure TMainForm.SetStylePanel();
begin
  FreeAndNil(StylePanel);
  StylePanel := TPanel.Create(ToolsPanel);
  StylePanel.Parent := ToolsPanel;
  StylePanel.Width := ToolsPanel.Width;
  StylePanel.Height := 400; //Как убрать константу?
  StylePanel.Top := ((length(gToolClasses) + length(gFigureClasses) + 1) div 2) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
end;

{Scroll bars constructor}
procedure TMainForm.SetScrollBars();
var
  Figure: TFigure;
  XMin, XMax: Integer;
  YMin, YMax: Integer;
  CanvasCorner: TDoublePoint;
begin
  XMin := Round(Min(gCanvasOffset.mX - CANVAS_OFFSET_BORDER_SIZE,
                    -CANVAS_OFFSET_BORDER_SIZE));
  XMax := Round(Max(gCanvasOffset.mX + CANVAS_OFFSET_BORDER_SIZE,
                    CANVAS_OFFSET_BORDER_SIZE));
  YMin := Round(Min(gCanvasOffset.mX - CANVAS_OFFSET_BORDER_SIZE,
                    -CANVAS_OFFSET_BORDER_SIZE));
  YMax := Round(Max(gCanvasOffset.mX + CANVAS_OFFSET_BORDER_SIZE,
                     CANVAS_OFFSET_BORDER_SIZE));

  CanvasCorner := CanvasToWorld(gCanvasWidth, gCanvasHeight);

  for Figure in mFigures do
  begin
    XMin := Min(XMin, Round(Figure.TopLeftBorder.mX - CANVAS_OFFSET_BORDER_SIZE));
    XMax := Max(XMax, Round(gCanvasOffset.mX + Figure.BottomRightBorder.mX - CanvasCorner.mX + CANVAS_OFFSET_BORDER_SIZE));
    YMin := Min(YMin, Round(Figure.TopLeftBorder.mY - CANVAS_OFFSET_BORDER_SIZE));
    YMax := Max(YMax, Round(gCanvasOffset.mY + Figure.BottomRightBorder.mY - CanvasCorner.mY + CANVAS_OFFSET_BORDER_SIZE));
  end;

  HorScrollBar.Min := XMin;
  HorScrollBar.Max := XMax;
  VerScrollBar.Min := YMin;
  VerScrollBar.Max := YMax;

  HorScrollBar.Position := Round(gCanvasOffset.mX);
  VerScrollBar.Position := Round(gCanvasOffset.mY);

  HorScrollBar.PageSize := Round((CanvasCorner.mX-gCanvasOffset.mX) / (XMax-XMin));
  VerScrollBar.PageSize := Round((CanvasCorner.mY-gCanvasOffset.mY) / (YMax-YMin));
end;

{Menu actions}
procedure TMainForm.MenuItemSetDefaultClick(Sender: TObject);
begin
  ZoomPoint(DoubleToPoint(0, 0), 1);
  MainForm.Invalidate;
end;

procedure TMainForm.closeForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  PaintBox.Free;
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
const
  ABOUT = 'About.txt';
var
  showText: TStringList;

begin
  showText := TStringList.Create;
  showText.LoadFromFile(ABOUT);
  showMessage(showText.Text);
  showText.Free;
end;

procedure TMainForm.ClearAllMenuItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := low(mFigures) to high(mFigures) do
    FreeAndNil(mFigures[i]);
  SetLength(mFigures, 0);
  MainForm.invalidate;
end;

{OnEvent actions}
procedure TMainForm.Scroll(Sender: TObject;
                          ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  gCanvasOffset.mX := HorScrollBar.Position;
  gCanvasOffset.mY := VerScrollBar.Position;
  MainForm.Invalidate;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  gCanvasWidth := MainForm.Width;
  gCanvasHeight := MainForm.Height;
  SetScrollBars;
end;

procedure TMainForm.FigureBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
begin
  Btn := Sender as TSpeedButton;
  mCurrentFigure := gFigureClasses[Btn.Tag];
  mCurrentAction:= ACTION_FIGURE;
  SetStylePanel();
  mCurrentFigure.SetParameters(StylePanel);
end;

procedure TMainFOrm.ToolBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
begin
  Btn := Sender as TSpeedButton;
  mCurrentTool := gToolClasses[Btn.Tag];
  mCurrentAction := ACTION_TOOL;
  FreeAndNil(StylePanel);
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y: integer);
var
  WorldStartPoint: TDoublePoint;

begin
    mIsDrawing := true;
    WorldStartPoint := CanvasToWorld(x, y);
    case mCurrentAction of
      ACTION_FIGURE:
      begin
        case Button of
          mbLeft:
          begin
            SetLength(mFigures, length(mFigures) + 1);
            mFigures[high(mFigures)] := mCurrentFigure.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button)
          end;
          mbRight:
          begin
            if length(mFigures) > 0 then
            begin
              FreeAndNil(mFigures[high(mFigures)]);
              SetLength(mFigures, length(mFigures) - 1);
            end;
          end;
        end;
      end;
      ACTION_TOOL:
      begin
        case Button of
          mbLeft:
          begin
            SetLength(mTools, length(mTools) + 1);
            mTools[high(mTools)] := mCurrentTool.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
          end;
          mbRight:
          begin
              SetLength(mTools, length(mTools) + 1);
              mTools[high(mTools)] := mCurrentTool.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
          end;
        end;
      end;
    end;

  MainForm.Invalidate;
  SetScrollBars();
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
                                      X, Y: integer);
begin
  if (mIsDrawing) then
  begin
    case mCurrentAction of
      ACTION_FIGURE: if (length(mFigures) > 0) and (shift = [ssLeft]) then     //Убрать срабатывание при нажатии на колесо мышки
                      mFigures[high(mFigures)].Update(x, y);
      ACTION_TOOL: mTools[high(mTools)].Update(x, y);
    end;

    MainForm.Invalidate;
    SetScrollBars();
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: integer);
begin
    mIsDrawing := false;
    If mCurrentAction = ACTION_TOOL then
      mTools[High(mTools)].MouseUp(x, y)
    else if (mCurrentAction = ACTION_FIGURE) and (Button = mbLeft) then
      mFigures[high(mFigures)].Update(x, y);
    MainForm.Invalidate;
end;

procedure TMainForm.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    ZoomPoint(CanvasToWorld(MousePos), gScale * 2)
  else
    ZoomPoint(CanvasToWorld(MousePos), gScale / 2);
  MainForm.Invalidate;
  SetScrollBars();
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  Figure: TFigure;
begin
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);

  for Figure in mFigures do
    Figure.Paint(PaintBox.Canvas);
end;

end.
