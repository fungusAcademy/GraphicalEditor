unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Buttons, StdCtrls, ColorBox, ufigures, uTools, uCoordinates, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
    HorScrollBar: TScrollBar;
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
    VerScrollBar: TScrollBar;
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
  end;

  TAction = (ACTION_FIGURE, ACTION_TOOL);

const
  BTN_SIZE = 40;
  BTN_MARGIN = 8;
  BTN_PADDING = 1;
  ABOUT = 'About.txt';
  START_PEN_COLOR: TColor = clBlack;
  START_BRUSH_COLOR: TColor = clWhite;
  START_PEN_STYLE: integer = 0;
  START_BRUSH_STYLE: integer = 1;
  START_PEN_WIDTH: integer = 1;

var
  MainForm: TMainForm;
  gIsDrawing: boolean;
  gCurrentFigure: TFigureClass;
  gCurrentTool: TToolClass;
  gFigures: array of TFigure;
  gTools: array of TTool;
  gCurrentAction: TAction;

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
  gIsDrawing := False;

  TFigure.sPenColor:= START_PEN_COLOR;
  TFigure.sBrushColor := START_BRUSH_COLOR;
  TFigure.sPenWidth:= START_PEN_WIDTH;
  TFigure.sPenStyle:= START_PEN_STYLE;
  TFIgure.sBrushStyle:= START_BRUSH_STYLE;

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
    CurrentIcon.LoadFromFile(gFigureClasses[i].ClassName + '.png');
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
    CurrentIcon.LoadFromFile(gToolClasses[i].ClassName + '.png');
    Btn.Glyph := CurrentIcon.Bitmap;
    CurrentIcon.Free;

    Btn.Left := ((i + 5) mod iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Top  := ((i + 5) div iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING; //КОСТЫЛЬ
    Btn.Width := BTN_SIZE + BTN_MARGIN;
    Btn.Height := Btn.Width;
  end;

  TSpeedButton(ToolsPanel.Controls[0]).Click;
  TSpeedButton(ToolsPanel.Controls[0]).Down:= true;

  gCanvasWidth := PaintBox.Width;
  gCanvasHeight := PaintBox.Height;
end;

procedure TMainForm.SetStylePanel();
begin
  FreeAndNil(StylePanel);
  StylePanel := TPanel.Create(ToolsPanel);
  StylePanel.Parent := ToolsPanel;
  StylePanel.Width := ToolsPanel.Width;
  StylePanel.Height := 300; //Как убрать константу?
  StylePanel.Top := ((length(gToolClasses) + length(gFigureClasses) + 1) div 2) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
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
  for i := low(gFigures) to high(gFigures) do
    FreeAndNil(gFigures[i]);
  //Стоит ли чистить массив инструментов?
  SetLength(gFigures, 0);
  MainForm.invalidate;
end;

{OnEvent actions}
procedure TMainForm.FigureBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
begin
  Btn := Sender as TSpeedButton;
  gCurrentFigure := gFigureClasses[Btn.Tag];
  gCurrentAction:= ACTION_FIGURE;
  SetStylePanel();
  gCurrentFigure.SetParameters(StylePanel);
end;

procedure TMainFOrm.ToolBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
begin
  Btn := Sender as TSpeedButton;
  gCurrentTool := gToolClasses[Btn.Tag];
  gCurrentAction := ACTION_TOOL;
  FreeAndNil(StylePanel);
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y: integer);
var
  WorldStartPoint: TDoublePoint;

begin
    gIsDrawing := true;
    WorldStartPoint := CanvasToWorld(x, y);
    case gCurrentAction of
      ACTION_FIGURE:
      begin
        case Button of
          mbLeft:
          begin
            SetLength(gFigures, length(gFigures) + 1);
            gFigures[high(gFigures)] := gCurrentFigure.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button)
          end;
          mbRight:
          begin
            if length(gFigures) > 0 then
            begin
              FreeAndNil(gFigures[high(gFigures)]);
              SetLength(gFigures, length(gFigures) - 1);
            end;
          end;
        end;
      end;
      ACTION_TOOL:
      begin
        case Button of
          mbLeft:
          begin
            SetLength(gTools, length(gTools) + 1);
            gTools[high(gTools)] := gCurrentTool.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
          end;
          mbRight:
          begin
              SetLength(gTools, length(gTools) + 1);
              gTools[high(gTools)] := gCurrentTool.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
          end;
        end;
      end;
    end;

  MainForm.Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
                                      X, Y: integer);
begin
  if (gIsDrawing) then
  begin
    case gCurrentAction of
      ACTION_FIGURE: if (length(gFigures) > 0) then     //Убрать срабатывание при нажатии на колесо мышки
                      gFigures[high(gFigures)].Update(x, y);
      ACTION_TOOL: gTools[high(gTools)].Update(x, y);
    end;

    MainForm.Invalidate;
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: integer);
begin
    gIsDrawing := false;
    If gCurrentAction = ACTION_TOOL then
      gTools[High(gTools)].MouseUp(x, y);

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
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  Figure: TFigure;
begin
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);

  for Figure in gFigures do
    Figure.Paint(PaintBox.Canvas);
end;

end.
