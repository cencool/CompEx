unit Lexer;

{$mode ObjFPC}{$H+}

interface



type
  token_name = (NONE, NUMBER, IDENTIFIER, PLUS, MINUS, MULTIPLY, DIVIDE, LEFT_PARENS,
    RIGHT_PARENS, SEMICOLON, LINE_END, UNKNOWN);

  TToken = record
    Name: token_name;
    lexeme: string;
  end;

var
  current_line: string = '';
  current_line_length: word = 0;
  current_line_number: word = 0;
  char_position: word = 0;
  current_char: string = '';
  peeked_char: string = '';
  lookahead: TToken = (Name: NONE; lexeme: '');

function read_line(var src_file: Text): boolean;
procedure read_char();
procedure peek_char();
procedure advance();
procedure match(checked_token: token_name);


implementation

uses
  Classes, SysUtils, LazUTF8, Character, Parser;

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
      lookahead.Name := PLUS;
      lookahead.lexeme := current_char;
      Exit();
    end;
    '-': begin
      lookahead.Name := MINUS;
      lookahead.lexeme := current_char;

      Exit();
    end;
    '*': begin
      lookahead.Name := MULTIPLY;
      lookahead.lexeme := current_char;

      Exit();
    end;
    '/': begin
      lookahead.Name := DIVIDE;
      lookahead.lexeme := current_char;

      Exit();
    end;
    '(': begin
      lookahead.Name := LEFT_PARENS;
      lookahead.lexeme := current_char;

      Exit();
    end;
    ')': begin
      lookahead.Name := RIGHT_PARENS;
      lookahead.lexeme := current_char;

      Exit();
    end;
    ';': begin
      lookahead.Name := SEMICOLON;
      lookahead.lexeme := current_char;

      Exit();
    end;
    '': begin
      lookahead.Name := LINE_END;
      lookahead.lexeme := current_char;

      Exit();
    end;
  end;
  {check for integer number }
  if IsDigit(utf8decode(current_char), 1) then
  begin
    lookahead.lexeme := current_char;
    peek_char();
    while (peeked_char <> '') and (IsDigit(utf8decode(peeked_char), 1)) and
      (peeked_char <> '.') do
    begin
      read_char();
      lookahead.lexeme := lookahead.lexeme + current_char;
      peek_char();
    end;
    if peeked_char = '.' then
    begin
      read_char();
      lookahead.lexeme := lookahead.lexeme + current_char;
      peek_char();
      while (peeked_char <> '') and (IsDigit(utf8decode(peeked_char), 1)) do
      begin
        read_char();
        lookahead.lexeme := lookahead.lexeme + current_char;
        peek_char();
      end;
    end;
    lookahead.Name := NUMBER;
    Exit();
  end;
  {check for identifier }
  if IsLetter(utf8decode(current_char), 1) then
  begin
    lookahead.lexeme := current_char;
    peek_char();
    while (peeked_char <> '') and (IsLetterOrDigit(UTF8Decode(peeked_char), 1)) do
    begin
      read_char();
      lookahead.lexeme := lookahead.lexeme + current_char;
      peek_char();
    end;
    lookahead.Name := IDENTIFIER;
    Exit();
  end;
  lookahead.Name := UNKNOWN;
  lookahead.lexeme := current_char;
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

    SharedNodePtr^.display_text := lookahead.lexeme;
    advance();    { #todo : preco netlaci samotne znamienka ? ale opakuje cislo  }

  end
  else
  begin
    WriteStr(token_name, checked_token);
    raise Exception.Create('Syntax error in line:' +
      IntToStr(current_line_number) + LineEnding + token_name +
      ' expected' + LineEnding + current_line);
  end;
end;



end.
