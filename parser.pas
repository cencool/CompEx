unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Lexer, Crt;

procedure parse(fileName: string);
procedure statements();
procedure expr();
procedure term();
procedure factor();
procedure expr_rest();
procedure term_rest();


implementation

procedure parse(fileName: string);
var
  src_file: Text;
begin
  assignFile(src_file, fileName);
  reset(src_file);
  while read_line(src_file) do
  begin
    advance();
    try
      statements();
    except
      on E: Exception do
      begin
        writeln(E.message);
        Exit;
      end;
    end;
  end;
  WriteLn('Parsing finished with OK result');
end;

procedure statements();
begin
  case lookahead of
    LINE_END: begin
      Exit();
    end;
    else
    begin
      expr();
      match(SEMICOLON);
      statements();
    end;
  end;

end;

procedure expr();
begin
  term();
  expr_rest();
end;

procedure term();
begin
  factor();
  term_rest();
end;

procedure expr_rest();
begin
  case lookahead of
    PLUS: begin
      match(PLUS);
      term();
      expr_rest();
    end;
    MINUS: begin
      match(MINUS);
      term();
      expr_rest();
    end;
  end;
end;

procedure term_rest();
begin
  case lookahead of
    MULTIPLY: begin
      match(MULTIPLY);
      factor();
      term_rest();
    end;
    DIVIDE: begin
      match(DIVIDE);
      factor();
      term_rest();
    end;
  end;
end;

procedure factor();
begin
  case lookahead of
    NUMBER: begin
      match(NUMBER);
    end;
    IDENTIFIER: begin
      match(IDENTIFIER);
    end;
    LEFT_PARENS: begin
      match(LEFT_PARENS);
      expr();
      match(RIGHT_PARENS);
    end;
    else
    begin
      raise Exception.Create('Syntax error in line: ' +
        IntToStr(current_line_number) + LineEnding +
        'Num or Identifier or Left parens expected' + LineEnding + current_line);
    end;
  end;
end;

end.
