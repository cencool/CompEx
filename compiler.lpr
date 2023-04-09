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
  if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');

  MyParser := TParser.Create;
  MyParser.parse(filename);
  FreeAndNil(MyParser);

end.
