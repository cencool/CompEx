unit Parser;

{$mode ObjFPC}{$H+}

interface



procedure parse(fileName: string);
procedure statements();
function expr(): single;
function term(): single;
function factor(): single;
function expr_rest(semi: single): single;
function term_rest(semi: single): single;


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
      write(' = ' + FloatToStr(expr()));
      match(SEMICOLON);
      Write(';');
      statements();
    end;
  end;

end;

function expr(): single;
begin
  Result := expr_rest(term());
  //expr_rest();
end;

function term(): single;
var
  i: single;
begin
  i := factor();
  Result := term_rest(i);
  //term_rest();
end;

function expr_rest(semi: single): single;
begin
  case lookahead.Name of
    PLUS: begin
      match(PLUS);
      Result := expr_rest(semi + term());
      //expr_rest();
      Write('+');
    end;
    MINUS: begin
      match(MINUS);
      Result := expr_rest(semi - term());
      //expr_rest();
      Write('-');
    end;
    else
      Result := semi;
  end;
end;

function term_rest(semi: single): single;
begin
  case lookahead.Name of
    MULTIPLY: begin
      match(MULTIPLY);
      Result := term_rest(semi * factor());
      //term_rest();
      Write('*');
    end;
    DIVIDE: begin
      match(DIVIDE);
      Result := term_rest(semi / factor());
      //term_rest();
      Write('/');
    end;
    else
      Result := semi;
  end;
end;

function factor(): single;
begin
  case lookahead.Name of
    NUMBER: begin
      match(NUMBER);
      Result := (strtofloat(lookahead.lexeme));
      Write(lookahead.lexeme + ' ');
    end;
    IDENTIFIER: begin
      match(IDENTIFIER);
      Result := 1.0;
      Write(lookahead.lexeme + ' ');
    end;
    LEFT_PARENS: begin
      match(LEFT_PARENS);
      Result := expr();
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
