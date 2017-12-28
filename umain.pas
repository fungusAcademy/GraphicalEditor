unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Buttons, StdCtrls, ColorBox, Spin, ufigures, uTools,
  uCoordinates, uProperty, Types;

type

  { TMainForm }

  TAction = (ACTION_FIGURE, ACTION_TOOL);

  TMainForm = class(TForm)
    MenuItemPaste: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemSetDefault: TMenuItem;
    MenuItemNew: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemShowHotKeys: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemRaiseUp: TMenuItem;
    MenuItemRaiseDown: TMenuItem;
    MenuItemClearSelected: TMenuItem;
    AreaAnimation: TTimer;
    HorScrollBar: TScrollBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    VerScrollBar: TScrollBar;
    MainMenu: TMainMenu;
    MenuItemEdit: TMenuItem;
    MenuItemClearAll: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemInfo: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    PaintBox: TPaintBox;
    ToolsPanel: TPanel;
    StylePanel: TPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormResize(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemRedoClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
    procedure MenuItemSetDefaultClick(Sender: TObject);
    procedure MenuItemClearAllClick(Sender: TObject);
    procedure createForm(Sender: TObject);
    procedure closeForm(Sender: TObject; var CloseAction: TCloseAction);

    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemUndoClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint;
      var Handled: boolean);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ToolBtnClick(Sender: TObject);
    procedure FigureBtnClick(Sender: TObject);
    procedure SetStylePanel();
    procedure SetScrollBars();
    procedure Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure AreaAnimationTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MenuItemClearSelectedClick(Sender: TObject);
    procedure MenuItemRaiseDownClick(Sender: TObject);
    procedure MenuItemRaiseUpClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure MenuItemShowHotKeysClick(Sender: TObject);
    procedure ToSavedState(IsSaved: boolean);
    procedure OnChange;
    function IsSavedDialog(): integer;
  private
    mIsSaved: boolean;
    mFileName: string;
    mIsDrawing: boolean;
    mCurrentFigure: TFigureClass;
    mCurrentTool: TToolClass;
    mCurrentAction: TAction;
  const
    BTN_SIZE = 40;
    BTN_MARGIN = 8;
    BTN_PADDING = 1;
    CANVAS_OFFSET_BORDER_SIZE = 10;
  public
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
  mIsDrawing := False;
  mIsSaved := True;
  mFileName := 'Untitled';
  MainForm.DoubleBuffered := True;
  MainForm.Caption := MFileName + ' - ' + ApplicationName;

  TProperty.SetDefault();

  iconsPerRow := ToolsPanel.Width div (BTN_SIZE + BTN_MARGIN + BTN_PADDING);
  ToolsPanel.Height := ((Length(gFigureClasses) + length(gToolClasses)) div
    iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING * 2;

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
    Btn.Top := (i div iconsPerRow) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
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

    Btn.Left := ((Length(gFigureClasses) + i) mod iconsPerRow) *
      (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Top := ((Length(gFigureClasses) + i) div iconsPerRow) *
      (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
    Btn.Width := BTN_SIZE + BTN_MARGIN;
    Btn.Height := Btn.Width;
  end;

  TSpeedButton(ToolsPanel.Controls[0]).Click;
  TSpeedButton(ToolsPanel.Controls[0]).Down := True;

  gCanvasWidth := PaintBox.Width;
  gCanvasHeight := PaintBox.Height;

  TFigure.ToSavedState := @ToSavedState;
  TFigure.OnChange := @OnChange;

  TFigure.InitHistory();
  MenuItemUndo.Enabled := False;
  MenuItemRedo.Enabled := False;

  SetScrollBars;
end;

procedure TMainForm.ToSavedState(IsSaved: boolean);
begin
  mIsSaved := IsSaved;
  if IsSaved then
    MainForm.Caption := mFileName + ' - ' + ApplicationName
  else
    MainForm.Caption := mFileName + ' - ' + ApplicationName + '(changed)';
end;

procedure TMainForm.OnChange();
begin
  mIsSaved := False;
  MainForm.Caption := mFileName + ' - ' + ApplicationName + '(changed)';
end;

{Style panel constructor}
procedure TMainForm.SetStylePanel();
begin
  FreeAndNil(StylePanel);
  StylePanel := TPanel.Create(ToolsPanel);
  StylePanel.Parent := ToolsPanel;
  StylePanel.Top := ((length(gToolClasses) + length(gFigureClasses) + 1) div 2) *
    (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
  StylePanel.AutoSize := True;
end;

{Scroll bars constructor}
procedure TMainForm.SetScrollBars();
var
  Figure: TFigure;
  XMin, XMax: integer;
  YMin, YMax: integer;
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

  for Figure in gFigures do
  begin
    XMin := Min(XMin, Round(Figure.TopLeftBorder.mX - CANVAS_OFFSET_BORDER_SIZE));
    XMax := Max(XMax, Round(gCanvasOffset.mX + Figure.BottomRightBorder.mX -
      CanvasCorner.mX + CANVAS_OFFSET_BORDER_SIZE));
    YMin := Min(YMin, Round(Figure.TopLeftBorder.mY - CANVAS_OFFSET_BORDER_SIZE));
    YMax := Max(YMax, Round(gCanvasOffset.mY + Figure.BottomRightBorder.mY -
      CanvasCorner.mY + CANVAS_OFFSET_BORDER_SIZE));
  end;

  HorScrollBar.Min := XMin;
  HorScrollBar.Max := XMax;
  VerScrollBar.Min := YMin;
  VerScrollBar.Max := YMax;

  HorScrollBar.Position := Round(gCanvasOffset.mX);
  VerScrollBar.Position := Round(gCanvasOffset.mY);

  HorScrollBar.PageSize := Round((CanvasCorner.mX - gCanvasOffset.mX) / (XMax - XMin));
  VerScrollBar.PageSize := Round((CanvasCorner.mY - gCanvasOffset.mY) / (YMax - YMin));
end;

{Menu actions}
function TMainForm.IsSavedDialog(): integer;
begin
  Result := MessageDlg('Save changes?', 'File has been modified, save changes?',
    mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    65: if shift = [ssCtrl] then
        MenuItemSelectAllClick(Sender);
    88: if shift = [ssCtrl] then
        MenuItemClearSelectedClick(Sender);
    68: if shift = [ssCtrl] then
        MenuItemRaiseDownClick(Sender);
    85: if shift = [ssCtrl] then
        MenuItemRaiseUpClick(Sender);
    87: if shift = [ssCtrl] then
        MenuItemClearAllClick(Sender);
    69: if shift = [ssCtrl] then
        MenuItemExitClick(Sender);
    83: if shift = [ssCtrl] then
        MenuItemSaveClick(Sender)
      else if shift = [ssCtrl, ssShift] then
        MenuItemSaveAsClick(Sender);
    78: if shift = [ssCtrl] then
        MenuItemNewClick(Sender);
    79: if shift = [ssCtrl] then
        MenuItemOpenClick(Sender);
    90: if shift = [ssCtrl] then
        MenuItemUndoClick(Sender)
      else if shift = [ssCtrl, ssShift] then
        MenuItemRedoClick(Sender);
    67: if shift = [ssCtrl] then
        MenuItemCopyClick(Sender);
    86: if shift = [ssCtrl] then
        MenuItemPasteClick(Sender);
  end;
  MainForm.Invalidate;
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

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
const
  LINK = 'About.txt';
var
  showText: TStringList;

begin
  showText := TStringList.Create;
  showText.LoadFromFile('Txt/' + LINK);
  ShowMessage(showText.Text);
  FreeAndNil(showText);
end;

procedure TMainForm.MenuItemUndoClick(Sender: TObject);
begin
  if TFigure.LoadPrev() then
      MenuItemRedo.Enabled := true;
  if gCurrent = 0 then
    MenuItemUndo.Enabled := False
  else
    MenuItemUndo.Enabled := True;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemRedoClick(Sender: TObject);
begin
  if TFigure.LoadNext() then
    MenuItemUndo.Enabled := true;
  if gCurrent = high(gHistory) then
    MenuItemRedo.Enabled := False
  else
    MenuItemRedo.Enabled := True;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemClearAllClick(Sender: TObject);
var
  i, k: integer;
begin
  k := 0;
  for i := 0 to high(gFigures) do
  begin
    FreeAndNil(gFigures[i]);
    k := 1;
  end;
  SetLength(gFigures, 0);
  if mFileName = 'Untitled' then
    ToSavedState(True)
  else
    ToSavedState(False);
  if k <> 0 then
  begin
    TFigure.PushToHistory;
    SavedToCurrent();
  end;
  MainForm.invalidate;
end;

procedure TMainForm.MenuItemClearSelectedClick(Sender: TObject);
var
  i, j, k: integer;
begin
  j := 0;
  k := 0;
  for i := 0 to high(gFigures) do
  begin
    if (gFigures[i].mIsSelected) then
    begin
      FreeAndNil(gFigures[i]);
      k := 1;
    end
    else
    begin
      gFigures[j] := gFigures[i];
      j := j + 1;
    end;
  end;
  setLength(gFigures, j);
  if j = 0 then
    MenuItemClearAllClick(Sender)
  else
  begin
    if k <> 0 then
    begin
      //SavedToCurrent();
      TFigure.PushToHistory;
      MainForm.Invalidate;
    end;
  end;
end;

procedure TMainForm.MenuItemRaiseDownClick(Sender: TObject); //Сделать через 1 цикл
var
  i, j, k: integer;
  Figure: TFigure;
begin
  k := 0;
  for i := high(gFigures) downto 0 do
  begin
    if (gFigures[i].mIsSelected) then
    begin
      for j := i downto k + 1 do
      begin
        Figure := gFigures[j];
        gFigures[j] := gFigures[j - 1];
        gFigures[j - 1] := Figure;
        k := j;
      end;
    end;
  end;
  if k <> 0 then
    TFigure.PushToHistory;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemRaiseUpClick(Sender: TObject);
var
  i, j, k: integer;
  Figure: TFigure;
begin
  k := high(gFigures);
  for i := 0 to high(gFigures) do
  begin
    if (gFigures[i].mIsSelected) then
    begin
      for j := i to k - 1 do
      begin
        Figure := gFigures[j];
        gFigures[j] := gFigures[j + 1];
        gFigures[j + 1] := Figure;
        k := j;
      end;
    end;
  end;
  if k <> 0 then
    TFigure.PushToHistory;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemSelectAllClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(gFigures) do
    gFigures[i].mIsSelected := True;
end;

procedure TMainForm.MenuItemShowHotKeysClick(Sender: TObject);
const
  LINK = 'HotKeys.txt';
var
  showText: TStringList;
begin
  showText := TStringList.Create;
  showText.LoadFromFile('Txt/' + LINK);
  ShowMessage(showText.Text);
  FreeAndNil(showText);
end;

procedure TMainForm.MenuItemOpenClick(Sender: TObject);
var
  Ans: integer;
begin
  if not mIsSaved then
  begin
    Ans := IsSavedDialog();
    if Ans = mrYes then
      MenuItemSaveAsClick(Sender)
    else if Ans = mrIgnore then
      Exit;
  end;
  if (OpenDialog.Execute) and (TFigure.LoadFile(OpenDialog.FileName)) then
  begin
    MFileName := OpenDialog.FileName;
    ToSavedState(True);
    TFigure.InitHistory();
  end;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemCopyClick(Sender: TObject);
begin
  TFigure.copySelected;
end;

procedure TMainForm.MenuItemPasteClick(Sender: TObject);
begin
  TFigure.pasteSelected;
end;

procedure TMainForm.MenuItemSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    TFigure.SaveToFile(SaveDialog.FileName);
    mFileName := SaveDialog.FileName;
    ToSavedState(True);
    SavedToCurrent();
  end;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemSaveClick(Sender: TObject);
begin
  if mFileName = 'Untitled' then
    MenuItemSaveAsClick(Sender)
  else
  begin
    TFigure.SaveToFile(mFileName);
    ToSavedState(True);
    SavedToCurrent();
  end;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemNewClick(Sender: TObject);
var
  Ans, i: integer;
begin
  if not mIsSaved then
  begin
    Ans := IsSavedDialog();
    if Ans = mrYes then
      MenuItemSaveAsClick(Sender)
    else if (Ans = mrIgnore) or (ans = mrCancel) then
      Exit;
  end;
  mFileName := 'Untitled';
  ToSavedState(True);
  for i := 0 to high(gFigures) do
    FreeAndNil(gFigures[i]);
  setLength(gFigures, 0);
  TFigure.InitHistory();
  MainForm.Invalidate;
end;

{Timer}
procedure TMainForm.AreaAnimationTimer(Sender: TObject);
var
  Figure: TFigure;
begin
  if length(gFigures) > 0 then
  begin
    for Figure in gFigures do
    begin
      if (Figure.mIsSelected) then
      begin
        if Figure.mI < 3 then
          Figure.mI := Figure.mI + 1
        else
          Figure.mI := 0;
      end;
    end;
    MainForm.Invalidate;
  end;
end;

{OnEvent actions}
procedure TMainForm.Scroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: integer);
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Ans: integer;
begin
  if mIsSaved then
    Exit;
  Ans := IsSavedDialog;
  if Ans = mrYes then
    MenuItemSaveAsCLick(Sender)
  else if Ans = mrNo then
    CanClose := True
  else
    CanClose := False;
end;

procedure TMainForm.FigureBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
begin
  Btn := Sender as TSpeedButton;
  mCurrentFigure := gFigureClasses[Btn.Tag];
  mCurrentAction := ACTION_FIGURE;
  SetStylePanel();
  mCurrentFigure.SetStyleButtons(StylePanel);
end;

procedure TMainForm.ToolBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
  i, j, num: integer;
begin
  Btn := Sender as TSpeedButton;
  mCurrentTool := gToolClasses[Btn.Tag];
  mCurrentAction := ACTION_TOOL;
  SetStylePanel();
  num := 0;
  for i := 0 to high(gFigures) - 1 do
  begin
    if gFigures[i].mIsSelected then
    begin
      gFigures[i].getParameters();
      for j := 0 to high(gFigures[i].mValidProperties) do
        if gFigures[i].mValidProperties[j] = True then
          num := max(num, j + 1);
    end;
    mCurrentTool.setParameters(stylepanel, num);
  end;
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  WorldStartPoint: TDoublePoint;
begin
  WorldStartPoint := CanvasToWorld(x, y);
  case mCurrentAction of
    ACTION_FIGURE:
    begin
      case Button of
        mbLeft:
        begin
          SetLength(gFigures, length(gFigures) + 1);
          gFigures[high(gFigures)] :=
            mCurrentFigure.Create(WorldStartPoint.mX, WorldStartPoint.mY, button);
          mIsSaved := False;
          MainForm.Caption := mFileName + ' - ' + ApplicationName + '(changed)';
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
          gTools[high(gTools)] :=
            mCurrentTool.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
          gTools[high(gTools)].MouseDown(x, y);
        end;
        mbRight:
        begin
          SetLength(gTools, length(gTools) + 1);
          gTools[high(gTools)] :=
            mCurrentTool.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
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
  case mCurrentAction of
    ACTION_FIGURE: if (length(gFigures) > 0) and (shift = [ssLeft]) then
        gFigures[high(gFigures)].Update(x, y);
    ACTION_TOOL: if (shift = [ssLeft]) or (shift = [ssRIght]) then
      begin
        gTools[high(gTools)].Update(x, y);
      end;
  end;
  MainForm.Invalidate;
  SetScrollBars();
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (mCurrentAction = ACTION_TOOL) and (button <> mbMiddle) then
    gTools[High(gTools)].MouseUp(x, y, shift, StylePanel)
  else if (mCurrentAction = ACTION_FIGURE) and (Button = mbLeft) then
  begin
    gFigures[high(gFigures)].MouseUp(x, y);
    MenuItemUndo.Enabled := True;
  end;
  MainForm.Invalidate;
end;

procedure TMainForm.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
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
  Tool: TTool;
begin
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  if length(gFigures) > 0 then
    for Figure in gFigures do
      Figure.Paint(PaintBox.Canvas);
  if length(gTools) > 0 then
    for Tool in gTools do
      if (Tool.mIsActive) then
        tool.DrawArea(PaintBox.Canvas);
end;


end.
