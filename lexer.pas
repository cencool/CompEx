unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type
  token_name = (NONE, NUMBER, IDENTIFIER, PLUS, MINUS, MULTIPLY, DIVIDE, LEFT_PARENS,
    RIGHT_PARENS, SEMICOLON, LINE_END, UNKNOWN);

  { TToken }

  TToken = class
    Name: token_name;
    Lexeme: string;
    constructor Create();
  end;


var
  current_line: string = '';
  peeked_line: string = '';
  current_line_length: word = 0;
  current_line_number: word = 0;
  peeked_line_length: word = 0;
  char_position: word = 0;
  current_char: string = '';
  peeked_char: string = '';
  lookahead: TToken;

function read_line(src: TStringList): boolean;
function peek_line(src: TStringList): boolean;
procedure read_char();
procedure peek_char();
procedure advance();
procedure match(checked_token: token_name);
procedure is_number(negative: boolean);


implementation

uses
  SysUtils, LazUTF8, Character, Parser;

function read_line(src: TStringList): boolean;
begin
  if (current_line_number < src.Count) then
  begin
    current_line := src.Strings[current_line_number];
    Inc(current_line_number);
    current_line_length := UTF8length(current_line);
    char_position := 0;
    Result := True;
  end
  else
  begin
    current_line_length := 0;
    current_line := '';
    char_position := 0;
    Result := False;
  end;

end;

function peek_line(src: TStringList): boolean;
begin
  if (current_line_number < src.Count) then
  begin
    peeked_line := src.Strings[current_line_number];
    peeked_line_length := UTF8Length(peeked_line);
    Result := True;
  end
  else
  begin
    Result := False;
    peeked_line := '';
    peeked_line_length := 0;
  end;
end;

procedure read_char();
begin
  Inc(char_position);
  if char_position <= current_line_length then
  begin
    current_char := utf8copy(current_line, char_position, 1);
  end
  else
  if (char_position - current_line_length) = 1 then current_char := LineEnding
  else
  if read_line(src_lines) then read_char()
  else
  begin
    current_char := '';
  end;
end;

procedure peek_char();
begin
  if char_position < current_line_length then
    peeked_char := UTF8copy(current_line, char_position + 1, 1)
  else
  if char_position = current_line_length then peeked_char := LineEnding
  else
  if peek_line(src_lines) then
  begin
    peeked_char := UTF8copy(peeked_line, 1, 1);
  end
  else
  begin
    peeked_char := '';
  end;

end;

procedure advance();
begin
  read_char();
  while (current_char <> '') and (IsWhiteSpace(utf8decode(current_char), 1) or
      (current_char = LineEnding)) do
    read_char();
  case current_char of
    '+': begin
      lookahead.Name := PLUS;
      lookahead.Lexeme := current_char;
      Exit();
    end;
    '-': begin
      lookahead.Name := MINUS;
      lookahead.Lexeme := current_char;
      Exit();
    end;
    '*': begin
      lookahead.Name := MULTIPLY;
      lookahead.Lexeme := current_char;

      Exit();
    end;
    '/': begin
      lookahead.Name := DIVIDE;
      lookahead.Lexeme := current_char;

      Exit();
    end;
    '(': begin
      lookahead.Name := LEFT_PARENS;
      lookahead.Lexeme := current_char;

      Exit();
    end;
    ')': begin
      lookahead.Name := RIGHT_PARENS;
      lookahead.Lexeme := current_char;

      Exit();
    end;
    ';': begin
      lookahead.Name := SEMICOLON;
      lookahead.Lexeme := current_char;

      Exit();
    end;
    '': begin
      lookahead.Name := LINE_END;
      lookahead.Lexeme := current_char;

      Exit();
    end;
  end;
  {check for integer number }

  if IsDigit(utf8decode(current_char), 1) then
  begin
    lookahead.Lexeme := current_char;
    peek_char();
    while (peeked_char <> '') and (IsDigit(utf8decode(peeked_char), 1)) and
      (peeked_char <> '.') do
    begin
      read_char();
      lookahead.Lexeme := lookahead.Lexeme + current_char;
      peek_char();
    end;
    {check for decimal number }
    if peeked_char = '.' then
    begin
      read_char();
      lookahead.Lexeme := lookahead.Lexeme + current_char;
      peek_char();
      while (peeked_char <> '') and (IsDigit(utf8decode(peeked_char), 1)) do
      begin
        read_char();
        lookahead.Lexeme := lookahead.Lexeme + current_char;
        peek_char();
      end;
    end;
    if not (IsLetter(UTF8Decode(peeked_char), 1)) then
    begin
      lookahead.Name := NUMBER;
      Exit();
    end;

  end;
  {check for identifier }
  if IsLetter(utf8decode(current_char), 1) then
  begin
    lookahead.Lexeme := current_char;
    peek_char();
    while (peeked_char <> '') and (IsLetterOrDigit(UTF8Decode(peeked_char), 1)) do
    begin
      read_char();
      lookahead.Lexeme := lookahead.Lexeme + current_char;
      peek_char();
    end;
    lookahead.Name := IDENTIFIER;
    Exit();
  end;
  lookahead.Name := UNKNOWN;
  lookahead.Lexeme := current_char;
  Exit();
end;

procedure match(checked_token: token_name);
var
  token_name: string;
  MyNodePtr: ParseNodePtr;
begin
  if checked_token = lookahead.Name then
  begin
    MyNodePtr := SharedNodePtr;
    MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));

    SharedNodePtr^.display_text := lookahead.Lexeme;
    advance();

  end
  else
  begin
    WriteStr(token_name, checked_token);
    raise Exception.Create('Syntax error in line:' +
      IntToStr(current_line_number) + LineEnding + token_name +
      ' expected' + LineEnding + current_line);
  end;
end;

procedure is_number(negative: boolean);  { #todo : solve negative number processing }
begin
  if IsDigit(utf8decode(current_char), 1) then
  begin
    if negative then lookahead.Lexeme := '-' + current_char
    else
      lookahead.Lexeme := current_char;
    peek_char();
    while (peeked_char <> '') and (IsDigit(utf8decode(peeked_char), 1)) and
      (peeked_char <> '.') do
    begin
      read_char();
      lookahead.Lexeme := lookahead.Lexeme + current_char;
      peek_char();
    end;
    {check for decimal number }
    if peeked_char = '.' then
    begin
      read_char();
      lookahead.Lexeme := lookahead.Lexeme + current_char;
      peek_char();
      while (peeked_char <> '') and (IsDigit(utf8decode(peeked_char), 1)) do
      begin
        read_char();
        lookahead.Lexeme := lookahead.Lexeme + current_char;
        peek_char();
      end;
    end;
    lookahead.Name := NUMBER;
    Exit();
  end;
end;

{ TToken }

constructor TToken.Create;
begin
  Name := NONE;
  Lexeme:='';
end;

end.
