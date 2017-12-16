unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Math,
  ExtCtrls, Buttons, StdCtrls, ColorBox, Spin, ufigures, uTools, uCoordinates,
  uProperty, Types;

type

  { TMainForm }

  TAction = (ACTION_FIGURE, ACTION_TOOL);

  TMainForm = class(TForm)
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
    procedure MenuItemNewClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure MenuItemSaveAsClick(Sender: TObject);
    procedure MenuItemSaveClick(Sender: TObject);
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
    procedure AreaAnimationTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItemClearSelectedClick(Sender: TObject);
    procedure MenuItemRaiseDownClick(Sender: TObject);
    procedure MenuItemRaiseUpClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure MenuItemShowHotKeysClick(Sender: TObject);
    function IsSavedDialog(): Integer;
  private
    mIsSaved: Boolean;
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
  mIsSaved := true;
  mFileName := 'Untitled';
  MainForm.DoubleBuffered := True;
  MainForm.Caption := MFileName + ' - ' + ApplicationName;

  TProperty.SetDefault();

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
  StylePanel.Top := ((length(gToolClasses) + length(gFigureClasses) + 1) div 2) * (BTN_SIZE + BTN_MARGIN) + BTN_PADDING;
  StylePanel.AutoSize := true;
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

  for Figure in gFigures do
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
function TMainForm.IsSavedDialog(): Integer;
begin
  Result:= MessageDlg('Save changes?', 'File has been modified, save changes?', mtConfirmation,
                   [mbYes, mbNo, mbCancel], 0);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case key of
    65: if shift = [ssCtrl] then MenuItemSelectAllClick(Sender);
    88: if shift = [ssCtrl] then MenuItemClearSelectedClick(Sender);
    68: if shift = [ssCtrl] then MenuItemRaiseDownClick(Sender);
    85: if shift = [ssCtrl] then MenuItemRaiseUpClick(Sender);
    87: if shift = [ssCtrl] then ClearAllMenuItemClick(Sender);
    69: if shift = [ssCtrl] then MenuItemExitClick(Sender);
    83: if shift = [ssCtrl] then MenuItemSaveClick(Sender)
        else if shift = [ssCtrl, ssShift] then MenuItemSaveAsClick(Sender);
    78: if shift = [ssCtrl] then MenuItemNewClick(Sender);
    79: if shift = [ssCtrl] then MenuItemOpenClick(Sender);
  end;
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
  showMessage(showText.Text);
  FreeAndNil(showText);
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

procedure TMainForm.MenuItemClearSelectedClick(Sender: TObject);
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to high(gFigures) do
    begin
      if (gFigures[i].mIsSelected) then
        FreeAndNil(gFigures[i])
      else
      begin
        gFigures[j] := gFigures[i];
        j := j + 1;
      end;
    end;
  setLength(gFigures, j);
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemRaiseDownClick(Sender: TObject); //Сделать через 1 цикл
var
  i, j, k: Integer;
  Figure: TFigure;
begin
  k := 0;
  for i := high(gFigures) downto 0 do
    begin
      if (gFigures[i].mIsSelected) then
        begin
          for j := i downto k + 1  do
          begin
            Figure := gFigures[j];
            gFigures[j] := gFigures[j-1];
            gFigures[j-1] := Figure;
            k := j
          end;
        end;
    end;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemRaiseUpClick(Sender: TObject);
var
  i, j, k: Integer;
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
            gFigures[j] := gFigures[j+1];
            gFigures[j+1] := Figure;
            k := j
          end;
        end;
    end;

  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemSelectAllClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(gFigures) do
    gFigures[i].mIsSelected := true;
end;

procedure TMainForm.MenuItemShowHotKeysClick(Sender: TObject);
const
  LINK = 'HotKeys.txt';
var
  showText: TStringList;
begin
  showText := TStringList.Create;
  showText.LoadFromFile('Txt/' + LINK);
  showMessage(showText.Text);
  FreeAndNil(showText);
end;

procedure TMainForm.MenuItemOpenClick(Sender: TObject);
var
  Ans: Integer;
begin
  if not mIsSaved then
  begin
    Ans:= IsSavedDialog();
    if Ans = mrYes then
       MenuItemSaveAsClick(Sender)
    else if Ans = mrIgnore then
      Exit;
  end;
  if (OpenDialog.Execute) and (TFigure.LoadFile(OpenDialog.FileName)) then
  begin
     MainForm.Caption:= OpenDialog.FileName + ' - ' + ApplicationName;
     MFileName:= OpenDialog.FileName;
     MIsSaved:= True;
  end;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    TFigure.Save(SaveDialog.FileName);
    MainForm.Caption:= SaveDialog.FileName + ' - ' + ApplicationName;
    mFileName:= SaveDialog.FileName;
    mIsSaved:= True;
  end;
  MainForm.Invalidate;
end;

procedure TMainForm.MenuItemSaveClick(Sender: TObject);
begin
  if mFileName = 'Untitled' then
     MenuItemSaveAsClick(sender)
  else
  begin
    TFigure.Save(mFileName);
    MainForm.Caption:= mFileName + ' - ' + ApplicationName;
    mIsSaved:= True;
  end;
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

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Ans: Integer;
begin
  if mIsSaved then
    Exit;
  Ans:= IsSavedDialog;
  if Ans = mrYes then
    MenuItemSaveAsCLick(Sender)
  else if Ans = mrNo then
    CanClose:= True
  else
    CanClose:= False;
end;


procedure TMainForm.MenuItemNewClick(Sender: TObject);
var
  Ans: Integer;
begin
  if not mIsSaved then
  begin
    Ans:= IsSavedDialog();
    if Ans = mrYes then
       MenuItemSaveAsClick(sender)
    else if Ans = mrIgnore then
       Exit;
  end;
  mFileName:= 'Untitled';
  MainForm.Caption:= mFileName + ' - ' + ApplicationName;
  mIsSaved:= True;
  SetLength(gFigures, 0);
  MainForm.Invalidate;
end;

procedure TMainForm.FigureBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
begin
  Btn := Sender as TSpeedButton;
  mCurrentFigure := gFigureClasses[Btn.Tag];
  mCurrentAction:= ACTION_FIGURE;
  SetStylePanel();
  mCurrentFigure.SetStyleButtons(StylePanel);
end;

procedure TMainForm.ToolBtnClick(Sender: TObject);
var
  Btn: TSpeedButton;
  i, j, num: Integer;
begin
  Btn := Sender as TSpeedButton;
  mCurrentTool := gToolClasses[Btn.Tag];
  mCurrentAction := ACTION_TOOL;
  SetStylePanel();
  num := 0;
  for i := 0 to high(gFigures)-1 do
  begin
    if gFigures[i].mIsSelected then
    begin
      gFigures[i].getParameters();
      for j := 0 to high(gFigures[i].mValidProperties) do
        if gFigures[i].mValidProperties[j] = true then
          num := max(num, j+1);
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
            gFigures[high(gFigures)] := mCurrentFigure.Create(WorldStartPoint.mX, WorldStartPoint.mY, button);
            mIsSaved := false;
            MainForm.Caption:= mFileName + ' - ' + ApplicationName + '(changed)';
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
            gTools[high(gTools)] := mCurrentTool.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
            gTools[high(gTools)].MouseDown(x, y);
          end;
          mbRight:
          begin
              SetLength(gTools, length(gTools) + 1);
              gTools[high(gTools)] := mCurrentTool.Create(WorldStartPoint.mX, WorldStartPoint.mY, Button);
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
        ACTION_FIGURE: if (length(gFigures) > 0) and  (shift = [ssLeft]) then
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
    If (mCurrentAction = ACTION_TOOL) and (button <> mbMiddle) then
      gTools[High(gTools)].MouseUp(x, y, shift, StylePanel)
    else if (mCurrentAction = ACTION_FIGURE) and (Button = mbLeft) and (shift = [ssLeft]) then
      gFigures[high(gFigures)].MouseUp(x, y);
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
  Tool: TTool;
begin
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  if length(gFigures) > 0 then
    for Figure in gFigures do
      Figure.Paint(PaintBox.Canvas);
  if length(gTools) > 0 then
    for Tool in gTools do
      if (Tool.mIsActive)
        then
          tool.DrawArea(PaintBox.Canvas);
end;


end.
