program compiler;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,
  { you can add units after this }
  SysUtils,
  Parser;


const
  filename = 'in.txt';

begin
  parse(filename);
end.


