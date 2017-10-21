unit ufigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls;

type

  TFigure = class
  private
    mPoints: array of TPoint;
  public
    constructor Create(x, y: integer);
    procedure Paint(Canvas: TCanvas); virtual; abstract;
    procedure Update(x, y: integer); virtual; abstract;
  end;

  TFigureClass = class of TFigure;

  TPolyline = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: integer); override;
  end;

  TLine = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: integer); override;
  end;

  TRectangle = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: integer); override;
  end;

  TEllipse = class(TFigure)
    procedure Paint(Canvas: TCanvas); override;
    procedure Update(x, y: integer); override;
  end;

var
  i: integer;

implementation

constructor TFigure.Create(x, y: integer);
begin
  SetLength(mPoints, 2);
  mPoints[0] := point(x, y);
  mPoints[1] := mPoints[0];
end;

procedure TPolyline.Paint(Canvas: TCanvas);
begin
  Canvas.Polyline(mPoints);
end;

procedure TPolyLine.Update(x, y: integer);
begin
  SetLength(mPoints, length(mPoints) + 1);
  mPoints[high(mPoints)] := point(x, y);
end;

procedure TLine.Paint(Canvas: TCanvas);
begin
  Canvas.Line(mPoints[0], mPoints[1]);
end;

procedure TLine.Update(x, y: integer);
begin
  mPoints[high(mPoints)] := point(x, y);
end;

procedure TRectangle.Paint(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(mPoints[0].x, mPoints[0].y, mPoints[1].x, mPoints[1].y);
end;

procedure TRectangle.Update(x, y: integer);
begin
  mPoints[high(mPoints)] := point(x, y);
end;

procedure TEllipse.Paint(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Ellipse(mPoints[0].x, mPoints[0].y, mPoints[1].x, mPoints[1].y);
end;

procedure TEllipse.Update(x, y: integer);
begin
  mPoints[high(mPoints)] := point(x, y);
end;

begin

end.

