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

  try
    MyParser := TParser.Create;
    MyParser.parse(filename);
    Writeln('PARSE TREE:');
    MyParser.PrintParseTree(MyParser.ParseRoot);
    WriteLn();
    Writeln('TRANSLATION TO POSTFIX');
    MyParser.SyntaxRoot.Gen;
    Writeln();
    FreeAndNil(MyParser);
  except
    on E: Exception do
    begin
      Writeln('Program exited with Error');
    end;
  end;

end.
