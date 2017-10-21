unit uFigureClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ufigures;

procedure registerTools(FigureClasses: array of TFigureClass);

var
  gFigureClasses: array of TFigureClass;

implementation

procedure registerTools(FigureClasses: array of TFigureClass);
var
  FigureClass: TFigureClass;

begin
  for FigureClass in FigureClasses do
  begin
    SetLength(gFigureClasses, length(gFigureClasses) + 1);
    gFigureClasses[high(gFigureClasses)] := FigureClass;
  end;
end;

initialization

registerTools([TPolyline, TLine,
              TRectangle, TEllipse]);
end.




