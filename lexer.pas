unit Lexer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Contnrs;

type
  TTokenTag = (NONE, NUMBER, IDENTIFIER, PLUS, MINUS, MULTIPLY, DIVIDE, LEFT_PARENS,
    RIGHT_PARENS, SEMICOLON, FILE_END, UNKNOWN, CURLY_LEFT, CURLY_RIGHT, TYPENAME);

  { TToken }

  TToken = class
    Tag: TTokenTag;
    Lexeme: string;
    Value: extended;
    LexemePosition: array of array [0..1] of integer;
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
    Words: TFPObjectHashTable;

    constructor Create(const SrcFileName: string);
    destructor Destroy; override;
    function ReadLine(): boolean;
    function PeekLine(src: TStringList): boolean;
    procedure ReadChar();
    procedure PeekChar();
    procedure Advance();
    procedure Match(checked_token: TTokenTag);
    procedure IsNumber(negative: boolean);
    function CreateWordsTable: TFPObjectHashTable;
    procedure WordsIterator(Item: TObject; const key: string; var Continue: boolean);


  end;



implementation

uses
  SysUtils, LazUTF8, Character;

constructor TLexer.Create(const SrcFileName: string);
begin
  SrcLines := TStringList.Create;
  SrcLines.LoadFromFile(SrcFileName);
  Lookahead := TToken.Create();
  Words := CreateWordsTable;
end;

destructor TLexer.Destroy;
begin
  FreeAndNil(SrcLines);
  FreeAndNil(Lookahead);
  FreeAndNil(Words);
  inherited Destroy;
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
  if ReadLine() then
  begin
    CharPosition := 0;
    ReadChar();
  end
  else
  begin
    CurrentChar := '';
    CharPosition := 0;
  end;
  PeekChar();
end;

{ #todo : isn't better to get peek character every time readchar is done ? }
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
var
  t: TToken;
  IdPosition: array [0..1] of integer;
begin
  ReadChar();

  while (CurrentChar <> '') and (IsWhiteSpace(utf8decode(CurrentChar), 1) or
      (CurrentChar = LineEnding)) do
    ReadChar();

  IdPosition[0] := CurrentLineNumber;
  IdPosition[1] := CharPosition;
  case CurrentChar of
    '+': begin
      Lookahead.Tag := PLUS;
      Lookahead.Lexeme := CurrentChar;
      Exit();
    end;
    '-': begin
      Lookahead.Tag := MINUS;
      Lookahead.Lexeme := CurrentChar;
      Exit();
    end;
    '*': begin
      Lookahead.Tag := MULTIPLY;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    '/': begin
      Lookahead.Tag := DIVIDE;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    '(': begin
      Lookahead.Tag := LEFT_PARENS;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    ')': begin
      Lookahead.Tag := RIGHT_PARENS;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    ';': begin
      Lookahead.Tag := SEMICOLON;
      Lookahead.Lexeme := CurrentChar;

      Exit();
    end;
    '{': begin
      Lookahead.Tag := CURLY_LEFT;
      Lookahead.Lexeme := CurrentChar;
      Exit;
    end;
    '}': begin
      Lookahead.Tag := CURLY_RIGHT;
      Lookahead.Lexeme := CurrentChar;
      Exit;
    end;
    '': begin
      Lookahead.Tag := NONE;
      Lookahead.Lexeme := CurrentChar;
      Exit();
    end;
  end;
  {check for integer number }

  if IsDigit(utf8decode(CurrentChar), 1) then
  begin
    Lookahead.Lexeme := CurrentChar;
    while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) and
      (PeekedChar <> '.') do
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
    end;
    {check for decimal number }
    if PeekedChar = '.' then
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) do
      begin
        ReadChar();
        Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      end;
    end;
    if not (IsLetter(UTF8Decode(PeekedChar), 1)) then
    begin
      Lookahead.Tag := NUMBER;
      Lookahead.Value := StrToFloat(Lookahead.Lexeme);
      Exit();
    end;

  end;
  {check for identifier }
  if IsLetter(utf8decode(CurrentChar), 1) then
  begin
    Lookahead.Lexeme := CurrentChar;
    while (PeekedChar <> '') and (IsLetterOrDigit(UTF8Decode(PeekedChar), 1)) do
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
    end;
    if Words.Items[Lookahead.Lexeme] <> nil then
    begin
      Lookahead.Tag := TToken(Words.Items[Lookahead.Lexeme]).Tag;
      insert(IdPosition, TToken(Words.Items[Lookahead.Lexeme]).LexemePosition, MaxInt);
    end
    else
    begin
      Lookahead.Tag := IDENTIFIER;
      t := TToken.Create();
      t.Lexeme := Lookahead.Lexeme;
      t.Tag := Lookahead.Tag;
      insert(IdPosition, t.LexemePosition, MaxInt);
      Words.Add(Lookahead.Lexeme, t);
    end;

    {todo: finalize token storage for identifiers }
    Exit();
  end;
  Lookahead.Tag := UNKNOWN;
  Lookahead.Lexeme := CurrentChar;
  Exit();
end;

procedure TLexer.Match(checked_token: TTokenTag);
var
  token_name: string;
begin
  if checked_token = Lookahead.Tag then
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
{ #todo : solve negative number lexing}
begin
  if IsDigit(utf8decode(CurrentChar), 1) then
  begin
    if negative then Lookahead.Lexeme := '-' + CurrentChar
    else
      Lookahead.Lexeme := CurrentChar;
    while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) and
      (PeekedChar <> '.') do
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
    end;
    {check for decimal number }
    if PeekedChar = '.' then
    begin
      ReadChar();
      Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) do
      begin
        ReadChar();
        Lookahead.Lexeme := Lookahead.Lexeme + CurrentChar;
      end;
    end;
    Lookahead.Tag := NUMBER;
    Exit();
  end;
end;

function TLexer.CreateWordsTable: TFPObjectHashTable;
type
  TWordList = array [0..2] of string;
var
  WordList: TWordList = ('int', 'char', 'bool');
  s: string;
  t: TToken;
begin
  Result := TFPObjectHashTable.Create();
  for s in WordList do
  begin
    t := TToken.Create();
    t.Lexeme := s;
    t.Tag := TYPENAME;
    Result.Add(s, t);
  end;

end;

procedure TLexer.WordsIterator(Item: TObject; const key: string; var Continue: boolean);
var
  i: integer;
  ar: array [0..1] of integer;
begin
  Write(TToken(Item).Lexeme, ': ');
  for ar in TToken(Item).LexemePosition do
  begin
    //identifier postions in src text
    Write('(', ar[0], ',', ar[1], ') ');
  end;
  writeln();
end;



{ TToken }

constructor TToken.Create;
begin
  Tag := NONE;
end;

end.
