unit Parser;

{$mode ObjFPC}{$H+}

interface



procedure parse(fileName: string);
procedure statements();
procedure expr();
procedure term();
procedure factor();
procedure expr_rest();
procedure term_rest();


implementation

uses
  Classes, SysUtils, Lexer;

procedure parse(fileName: string);
var
  src_file: Text;
begin
  assignFile(src_file, fileName);
  reset(src_file);
  while read_line(src_file) do
  begin
    Write(current_line + ' => ');
    advance();
    try
      statements();
    except
      on E: Exception do
      begin
        writeln();
        writeln(E.message);
        Exit;
      end;
    end;
  end;
  WriteLn('Parsing finished with OK result');
end;

procedure statements();
begin
  case lookahead.Name of
    LINE_END: begin
      writeln();
      Exit();
    end;
    else
    begin
      expr();
      match(SEMICOLON);
      Write(';');
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
  case lookahead.Name of
    PLUS: begin
      match(PLUS);
      term();
      expr_rest();
      Write('+');
    end;
    MINUS: begin
      match(MINUS);
      term();
      expr_rest();
      Write('-');
    end;
  end;
end;

procedure term_rest();
begin
  case lookahead.Name of
    MULTIPLY: begin
      match(MULTIPLY);
      factor();
      term_rest();
      Write('*');
    end;
    DIVIDE: begin
      match(DIVIDE);
      factor();
      term_rest();
      Write('/');
    end;
  end;
end;

procedure factor();
begin
  case lookahead.Name of
    NUMBER: begin
      match(NUMBER);
      Write(lookahead.lexeme + ' ');
    end;
    IDENTIFIER: begin
      match(IDENTIFIER);
      Write(lookahead.lexeme + ' ');
    end;
    LEFT_PARENS: begin
      match(LEFT_PARENS);
      expr();
      match(RIGHT_PARENS);
    end;
    else
    begin
      raise Exception.Create('Syntax error in : ' +
        IntToStr(current_line_number) + ',' + IntToStr(char_position) +
        LineEnding + 'Num or Identifier or Left parens expected' +
        LineEnding + current_line);
    end;
  end;
end;

end.
