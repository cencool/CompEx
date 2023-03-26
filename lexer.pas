unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type
  TTokenName = (NONE, NUMBER, IDENTIFIER, PLUS, MINUS, MULTIPLY, DIVIDE, LEFT_PARENS,
    RIGHT_PARENS, SEMICOLON, LINE_END, UNKNOWN);

  { TToken }

  TToken = class
    Name: TTokenName;
    Lexeme: string;
    constructor Create();
  end;


  { TLexer }

  TLexer = class

    SrcLines: TStringList;
    CurrentLine: string;
    PeekedLine: string;
    CurrentLineLength: word;
    CurrentLineNumber: word;
    PeekedLineLength: word;
    CharPosition: word;
    CurrentChar: string;
    PeekedChar: string;
    Lookahead: TToken;

    constructor Create(const SrcFileName: string);
    function ReadLine(): boolean;
    function PeekLine(src: TStringList): boolean;
    procedure ReadChar();
    procedure PeekChar();
    procedure Advance();
    procedure Match(checked_token: TTokenName);
    procedure IsNumber(negative: boolean);

  end;



implementation

uses
  SysUtils, LazUTF8, Character;

constructor TLexer.Create(const SrcFileName: string);
begin
  SrcLines := TStringList.Create;
  SrcLines.LoadFromFile(SrcFileName);
  CurrentLineNumber := 0;
  Lookahead := TToken.Create();
end;

function TLexer.ReadLine(): boolean;
begin
  if (CurrentLineNumber < SrcLines.Count) then
  begin
    CurrentLine := SrcLines.Strings[CurrentLineNumber];
    Inc(CurrentLineNumber);
    CurrentLineLength := UTF8length(CurrentLine);
    CharPosition := 0;
    Result := True;
  end
  else
  begin
    CurrentLineLength := 0;
    CurrentLine := '';
    CharPosition := 0;
    Result := False;
  end;

end;

function TLexer.PeekLine(src: TStringList): boolean;
begin
  if (CurrentLineNumber < src.Count) then
  begin
    PeekedLine := src.Strings[CurrentLineNumber];
    PeekedLineLength := UTF8Length(PeekedLine);
    Result := True;
  end
  else
  begin
    Result := False;
    PeekedLine := '';
    PeekedLineLength := 0;
  end;
end;

procedure TLexer.ReadChar();
begin
  Inc(CharPosition);
  if CharPosition <= CurrentLineLength then
  begin
    CurrentChar := utf8copy(CurrentLine, CharPosition, 1);
  end
  else
  if (CharPosition - CurrentLineLength) = 1 then CurrentChar := LineEnding
  else
  if ReadLine() then ReadChar()
  else
  begin
    CurrentChar := '';
  end;
end;

procedure TLexer.PeekChar();
begin
  if CharPosition < CurrentLineLength then
    PeekedChar := UTF8copy(CurrentLine, CharPosition + 1, 1)
  else
  if CharPosition = CurrentLineLength then PeekedChar := LineEnding
  else
  if PeekLine(SrcLines) then
  begin
    PeekedChar := UTF8copy(PeekedLine, 1, 1);
  end
  else
  begin
    PeekedChar := '';
  end;

end;

procedure TLexer.Advance();
begin
  ReadChar();
  while (CurrentChar <> '') and (IsWhiteSpace(utf8decode(CurrentChar), 1) or
      (CurrentChar = LineEnding)) do
    ReadChar();
  case CurrentChar of
    '+': begin
      Lookahead.Name := PLUS;
      Lookahead.Lexeme := CurrentChar;
      Exit();
    end;
    '-': begin
      Lookahead.Name := MINUS;
      Lookahead.Lexeme := CurrentChar;
      Exit();
    end;
    '*': begin
      Lookahead.Name := MULTIPLY;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    '/': begin
      Lookahead.Name := DIVIDE;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    '(': begin
      Lookahead.Name := LEFT_PARENS;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    ')': begin
      Lookahead.Name := RIGHT_PARENS;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    ';': begin
      Lookahead.Name := SEMICOLON;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    '': begin
      Lookahead.Name := LINE_END;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
  end;
  {check for integer number }

  if IsDigit(utf8decode(CurrentChar), 1) then
  begin
    Lookahead.Lexeme := CurrentChar;
    PeekChar();
    while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) and
      (PeekedChar <> '.') do
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      PeekChar();
    end;
    {check for decimal number }
    if PeekedChar = '.' then
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      PeekChar();
      while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) do
      begin
        ReadChar();
        Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
        PeekChar();
      end;
    end;
    if not (IsLetter(UTF8Decode(PeekedChar), 1)) then
    begin
      Lookahead.Name := NUMBER;
      Exit();
    end;

  end;
  {check for identifier }
  if IsLetter(utf8decode(CurrentChar), 1) then
  begin
    Lookahead.Lexeme := CurrentChar;
    PeekChar();
    while (PeekedChar <> '') and (IsLetterOrDigit(UTF8Decode(PeekedChar), 1)) do
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      PeekChar();
    end;
    Lookahead.Name := IDENTIFIER;
    Exit();
  end;
  Lookahead.Name := UNKNOWN;
  Lookahead.Lexeme := CurrentChar;
  Exit();
end;

procedure TLexer.Match(checked_token: TTokenName);
var
  token_name: string;
begin
  if checked_token = Lookahead.Name then
  begin

    Advance();

  end
  else
  begin
    WriteStr(token_name, checked_token);
    raise Exception.Create('Syntax error in line:' + IntToStr(CurrentLineNumber) +
      LineEnding + token_name + ' expected' + LineEnding + CurrentLine);
  end;
end;

procedure TLexer.IsNumber(negative: boolean);
{ #todo : solve negative number processing }
begin
  if IsDigit(utf8decode(CurrentChar), 1) then
  begin
    if negative then Lookahead.Lexeme := '-' + CurrentChar
    else
      Lookahead.Lexeme := CurrentChar;
    PeekChar();
    while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) and
      (PeekedChar <> '.') do
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      PeekChar();
    end;
    {check for decimal number }
    if PeekedChar = '.' then
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      PeekChar();
      while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) do
      begin
        ReadChar();
        Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
        PeekChar();
      end;
    end;
    Lookahead.Name := NUMBER;
    Exit();
  end;
end;

{ TToken }

constructor TToken.Create;
begin
  Name := NONE;
  Lexeme := '';
end;

end.
