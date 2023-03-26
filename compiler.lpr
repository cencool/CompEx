program compiler;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,
  { you can add units after this }
  SysUtils,
  Contnrs,
  LAZUTF8,
  Parser;

const
  filename = 'in.txt';
var

  MyParser: TParser;

begin
  MyParser := TParser.Create;
  MyParser.parse(filename);

end.
