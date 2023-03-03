program compiler;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Classes,
  { you can add units after this }
  SysUtils,
  LazUTF8,
  Character,
  Crt,
  Trees,
  Lexer,
  Parser;

var
  subor: Text;

const
  filename = 'in.txt';

begin
  assignfile(subor, filename);
  reset(subor);
  while read_line(subor) do
  begin
    advance();
    while lookahead <> EMPTY do
    begin
      writeln(lookahead);
      advance();
    end;
    writeln(lookahead);
  end;
  WriteLn('Press any key..');
  readkey();
end.


