program compiler;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,
  { you can add units after this }
  SysUtils,
  Contnrs,
  Parser;

var
  MyList: TStringList;
  i: integer;
  s : string;
const
  filename = 'in.txt';

begin
  parse(filename);
  MyList := TStringList.Create;
  MyList.LoadFromFile(filename);
  writeln(MyList.Capacity);
end.
