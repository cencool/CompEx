unit Lexer;

{$mode ObjFPC}{$H+}

interface


type
  token = (NONE, NUMBER, IDENTIFIER, PLUS, MINUS, MULTIPLY, DIVIDE, LEFT_PARENS,
    RIGHT_PARENS, UNKNOWN, LINE_END);

var
  current_line: string = '';
  current_line_length: word = 0;
  current_line_number: word = 0;
  char_position: word = 0;
  current_char: string = '';
  peeked_char: string = '';
  lookahead: token = NONE;

function read_line(var src_file: Text): boolean;
procedure read_char();
procedure peek_char();
procedure advance();
procedure match(checked_token: token);


implementation

uses
  Classes, SysUtils, LazUTF8, Character;

function read_line(var src_file: Text): boolean;

begin
  if not EOF(src_file) then
  begin
    ReadLn(src_file, current_line);
    Inc(current_line_number);
    current_line_length := length(current_line);
    char_position := 0;
    Result := True;
  end
  else
  begin
    current_line_length := 0;
    current_line := '';
    current_line_number := 0;
    char_position := 0;
    Result := False;
  end;

end;

procedure read_char();
begin
  Inc(char_position);
  if char_position <= current_line_length then
  begin
    current_char := current_line[char_position];
  end
  else
  begin
    current_char := '';
  end;
end;

procedure peek_char();
begin
  if char_position < current_line_length then
    peeked_char := current_line[char_position + 1]
  else
    peeked_char := '';

end;

procedure advance();
begin
  read_char();
  while (current_char <> '') and (IsWhiteSpace(utf8decode(current_char), 1)) do
    read_char();
  case current_char of
    '+': begin
      lookahead := PLUS;
      Exit();
    end;
    '-': begin
      lookahead := MINUS;
      Exit();
    end;
    '*': begin
      lookahead := MULTIPLY;
      Exit();
    end;
    '/': begin
      lookahead := DIVIDE;
      Exit();
    end;
    '(': begin
      lookahead := LEFT_PARENS;
      Exit();
    end;
    ')': begin
      lookahead := RIGHT_PARENS;
      Exit();
    end;
    '': begin
      lookahead := LINE_END;
      Exit();
    end;
  end;
  {check for integer number }
  if IsDigit(utf8decode(current_char), 1) then
  begin
    peek_char();
    while (peeked_char <> '') and (IsDigit(utf8decode(peeked_char), 1)) do
    begin
      read_char();
      peek_char();
    end;
    lookahead := NUMBER;
    Exit();
  end;
  {check for identifier }
  if IsLetter(utf8decode(current_char), 1) then
  begin
    peek_char();
    while (peeked_char <> '') and (IsLetterOrDigit(UTF8Decode(peeked_char), 1)) do
    begin
      read_char();
      peek_char();
    end;
    lookahead := IDENTIFIER;
    Exit();
  end;
  lookahead := UNKNOWN;
  Exit();
end;

procedure match(checked_token: token);
begin
  if checked_token = lookahead then
    advance()
  else
  begin
    raise Exception.Create('Syntax error in line:' + IntToStr(current_line_number));
  end;
end;



end.
