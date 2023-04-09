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
    constructor Create();
  end;

  { TTokenSymbol }

  TTokenSymbol = class(TToken)
    constructor Create(ATag: TTokenTag; ASymbol: string);
  end;

  { TTokenNumber }

  TTokenNumber = class(TToken)
    Value: extended;
    constructor Create(ALexeme: string);
  end;

  { TTokenWord }

  TTokenWord = class(TToken)
    LexemePosition: array of array [0..1] of integer;
    constructor Create(ATag: TTokenTag; AWord: string);
  end;




  { TLexer }

  TLexer = class
  type
    TPosition = array [0..1] of integer;

  private
    FLookahead: TToken;
    procedure setLookahead(AValue: TToken);
  public
    SrcLines: TStringList;
    CurrentLine: string;
    PeekedLine: string;
    CurrentLineLength: word;
    CurrentLineNumber: word;
    PeekedLineLength: word;
    CharPosition: word;
    CurrentChar: string;
    PeekedChar: string;
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
    function InitWordsTable: TFPObjectHashTable;
    procedure WordsIterator(Item: TObject; const key: string; var Continue: boolean);
    procedure StoreLexemePosition(APosition: TPosition; AToken: TTokenWord);
    property Lookahead: TToken read FLookahead write setLookahead;


  end;



implementation

uses
  SysUtils, LazUTF8, Character;

{ TTokenSymbol }

constructor TTokenSymbol.Create(ATag: TTokenTag; ASymbol: string);
begin
  inherited Create;
  Tag := ATag;
  Lexeme := ASymbol;
end;

{ TTokenWord }

constructor TTokenWord.Create(ATag: TTokenTag; AWord: string);
begin
  //inherited Create(ATag);
  Tag := ATag;
  Lexeme := AWord;
end;

{ TTokenNumber }

constructor TTokenNumber.Create(ALexeme: string);
begin
  inherited Create;
  Tag := NUMBER;
  Value := StrToFloat(ALexeme);
  Lexeme:= ALexeme;
end;


procedure TLexer.setLookahead(AValue: TToken);
begin
  if FLookahead = AValue then Exit;
  FreeAndNil(FLookahead);
  FLookahead := AValue;
end;

constructor TLexer.Create(const SrcFileName: string);
begin
  SrcLines := TStringList.Create;
  SrcLines.LoadFromFile(SrcFileName);
  Lookahead := TToken.Create();
  Words := InitWordsTable;
end;

destructor TLexer.Destroy;
begin
  FreeAndNil(SrcLines);
  FreeAndNil(FLookahead);
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

{ #done : isn't better to get peek character every time readchar is done ? }
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
  t: TTokenWord = nil;
  s: string = '';
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
      Lookahead := TTokenSymbol.Create(PLUS, '+');
      Exit();
    end;
    '-': begin
      Lookahead := TTokenSymbol.Create(MINUS, '-');
      Exit();
    end;
    '*': begin
      Lookahead := TTokenSymbol.Create(MULTIPLY, '*');
      Exit();
    end;
    '/': begin
      Lookahead := TTokenSymbol.Create(DIVIDE, '/');
      Exit();
    end;
    '(': begin
      Lookahead := TTokenSymbol.Create(LEFT_PARENS, '(');
      Exit();
    end;
    ')': begin
      Lookahead := TTokenSymbol.Create(RIGHT_PARENS, ')');
      Exit();
    end;
    ';': begin
      Lookahead := TTokenSymbol.Create(SEMICOLON, ';');
      Exit();
    end;
    '{': begin
      Lookahead := TTokenSymbol.Create(CURLY_LEFT, '{');
      Exit();
    end;
    '}': begin
      Lookahead := TTokenSymbol.Create(CURLY_RIGHT, '}');
      Exit;
    end;
    '': begin
      Lookahead := TTokenSymbol.Create(NONE, '');
      Exit();
    end;
  end;
  {check for integer number }

  if IsDigit(utf8decode(CurrentChar), 1) then
  begin
    s := CurrentChar;
    while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) and
      (PeekedChar <> '.') do
    begin
      ReadChar();
      s := s + CurrentChar;
    end;
    {check for decimal number }
    if PeekedChar = '.' then
    begin
      ReadChar();
      s := s + CurrentChar;
      while (PeekedChar <> '') and (IsDigit(utf8decode(PeekedChar), 1)) do
      begin
        ReadChar();
        s := s + CurrentChar;
      end;
    end;
    if not (IsLetter(UTF8Decode(PeekedChar), 1)) then
    begin
      Lookahead := TTokenNumber.Create(s);
      Exit();
    end;

  end;
  {check for identifier }
  if IsLetter(utf8decode(CurrentChar), 1) then
  begin
    s := CurrentChar;
    while (PeekedChar <> '') and (IsLetterOrDigit(UTF8Decode(PeekedChar), 1)) do
    begin
      ReadChar();
      s := s + CurrentChar;
    end;

    t := TTokenWord(Words.Items[s]);
    if t <> nil then   // identifier exists ?
    begin
      Lookahead := TTokenWord.Create(t.tag, s);
      StoreLexemePosition(IdPosition, t); // update word table entry
      StoreLexemePosition(IdPosition, TTokenWord(Lookahead));  //and  to new Lookahead
    end
    else
    begin
      Lookahead := TTokenWord.Create(IDENTIFIER, s);
      t := TTokenWord.Create(IDENTIFIER, s);
      StoreLexemePosition(IdPosition, t); // update word table entry
      StoreLexemePosition(IdPosition, TTokenWord(Lookahead));  //and  to new Lookahead
      Words.Add(s, t);
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

function TLexer.InitWordsTable: TFPObjectHashTable;
type
  TWordList = array [0..2] of string;
var
  WordList: TWordList = ('int', 'char', 'bool');
  s: string;
  t: TTokenWord;
begin
  Result := TFPObjectHashTable.Create();
  for s in WordList do
  begin
    t := TTokenWord.Create(TYPENAME, s);
    Result.Add(s, t);
  end;

end;

procedure TLexer.WordsIterator(Item: TObject; const key: string; var Continue: boolean);
var
  ar: array [0..1] of integer;
begin
  Continue := True;
  Write(key, ': ');

  for ar in TTokenWord(Item).LexemePosition do
  begin
    //identifier postions in src text line, col
    Write('(', ar[0], ',', ar[1], ') ');
  end;
  writeln();
end;

procedure TLexer.StoreLexemePosition(APosition: TPosition; AToken: TTokenWord);
begin
  insert(APosition, AToken.LexemePosition, MaxInt);
end;



{ TToken }

constructor TToken.Create;
begin
  Tag := NONE;
end;

end.
