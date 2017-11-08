unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Buttons, StdCtrls, ufigures, uTools, uCoordinates, Types;

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

  private
    { private declarations }
  public
    { public declarations }
  end;

const
  BTN_SIZE = 40;
  BTN_MARGIN = 8;
  BTN_PADDING = 1;
  ABOUT = 'About.txt';
  CANVAS_OFFSET_BORDER_SIZE = 10;

var
  MainForm: TMainForm;
  gIsDrawing: boolean;
  gCurrentFigure: TFigureClass;
  gFigures: array of TFigure;

implementation

{$R *.lfm}

{ TMainForm }
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

  iconsPerRow := ToolsPanel.Width div (BTN_SIZE + BTN_MARGIN + BTN_PADDING);
  ToolsPanel.Height := (Length(gFigureClasses) div iconsPerRow) *
    (BTN_SIZE + BTN_MARGIN) + BTN_PADDING * 2;

  for i := low(gFigureClasses) to high(gFigureClasses) do
  begin
    Btn := TSpeedButton.Create(ToolsPanel);
    Btn.Parent := ToolsPanel;
    Btn.Name := gFigureClasses[i].ClassName + 'Button';
    Btn.GroupIndex := 1;
    Btn.Tag := i;
    Btn.OnClick := @ToolBtnClick;

    CurrentIcon := TPicture.Create;
    CurrentIcon.LoadFromFile(gFigureClasses[i].ClassName + '.png');
    Btn.Glyph := CurrentIcon.Bitmap;
    CurrentIcon.Free;

    Btn.Left := (i mod iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Top  := (i div iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Width := BTN_SIZE + BTN_MARGIN;
    Btn.Height := Btn.Width;
  end;
  TSpeedButton(ToolsPanel.Controls[0]).Click;

  gCanvasWidth := PaintBox.Width;
  gCanvasHeight := PaintBox.Height;
end;

procedure TMainForm.MenuItemSetDefaultClick(Sender: TObject);
begin
  ZoomPoint(DoubleToPoint(0, 0), 1);
  MainForm.Invalidate;
end;

procedure TMainForm.closeForm(Sender: TObject; var CloseAction: TCloseAction);
begin
  PaintBox.Free;
end;

procedure TMainForm.ToolBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
begin
  Btn := Sender as TSpeedButton;
  gCurrentFigure := gFigureClasses[Btn.Tag];
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
  SetLength(gFigures, 0);
  MainForm.invalidate;
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y: integer);
var
  WorldStartPoint: TDoublePoint;

begin
  if Button = mbLeft then
  begin
    gIsDrawing := true;
    SetLength(gFigures, length(gFigures) + 1);
    WorldStartPoint := CanvasToWorld(x, y);
    gFigures[high(gFigures)] := gCurrentFigure.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
    MainForm.Invalidate;
  end;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
                                      X, Y: integer);
begin
  if (gIsDrawing) then
  begin
    gFigures[high(gFigures)].Update(x, y);
    MainForm.Invalidate;
  end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
                                    Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    gIsDrawing := false;
    gFigures[High(gFigures)].MouseUp(x, y);
    MainForm.Invalidate;
  end;
end;

procedure TMainForm.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    ZoomPoint(CanvasToWorld(MousePos), gScale*2)
  else
    ZoomPoint(CanvasToWorld(MousePos), gScale/2);
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
