unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type

  { TParseNode }

  TParseNode = class
    Name: string;
    DisplayText: string;
    ChildrenList: TList;
    constructor Create();
    destructor Destroy; override;
    procedure Link(Node: TParseNode);
  end;

  TSyntaxNode = class
    Name: string;
    DisplayText: string;
    Value: double;
    ChildrenList: TList;
  end;

  { TNodes }

  TNodes = class
    ParseNode: TParseNode;
    SyntaxNode: TSyntaxNode;
    constructor Create;
  end;

  { TParser }

  TParser = class
    NewNode: TParseNode;
    ParseRoot: TParseNode;
    { #done : add freeing objects }

    constructor Create;
    destructor Destroy; override;
    procedure CreateNewNode;
    procedure CreateNewNodeAndLink(Node: TParseNode);
    procedure parse(FileNameToParse: string);
    procedure statements();
    procedure expr();
    procedure term();
    procedure factor();
    procedure expr_rest();
    procedure term_rest();
    procedure PrintParseTree(Node: TParseNode; space_count: word);
    procedure FreeParseTree(Node: TParseNode);
  end;


implementation

uses
  SysUtils, Lexer;

var
  Lex: TLexer;

{ TNodes }

constructor TNodes.Create;
begin
  ParseNode := TParseNode.Create;
  SyntaxNode := TSyntaxNode.Create;
end;

procedure TParser.PrintParseTree(Node: TParseNode; space_count: word);
var
  ChildrenCount: integer = 0;
  i: integer = 0;
begin
  ChildrenCount := Node.ChildrenList.Count;
  for i := 1 to space_count do
  begin
    Write(' ');
  end;
  writeln(Node.DisplayText);
  for i := 0 to (ChildrenCount - 1) do
  begin
    PrintParseTree(TParseNode(Node.ChildrenList.Items[i]), space_count + 1);
  end;
end;

procedure TParser.FreeParseTree(Node: TParseNode);
var
  i: integer;
  ChildrenCount: integer = 0;
begin
  if Node <> nil then
  begin

    ChildrenCount := Node.ChildrenList.Count;
    for i := 0 to ChildrenCount - 1 do
    begin
      FreeParseTree(TParseNode(Node.ChildrenList.Items[i]));
    end;
    FreeAndNil(Node);

  end;

end;

constructor TParser.Create;
begin
end;

destructor TParser.Destroy;
begin
  FreeParseTree(ParseRoot);
  inherited Destroy;
end;

procedure TParser.CreateNewNode;
begin
  NewNode := TParseNode.Create();
end;

procedure TParser.CreateNewNodeAndLink(Node: TParseNode);
begin
  CreateNewNode;
  Node.Link(NewNode);
end;

procedure TParser.parse(FileNameToParse: string);

begin
  FreeParseTree(ParseRoot);
  CreateNewNode;
  ParseRoot := NewNode;
  ParseRoot.DisplayText := 'Program';
  CreateNewNodeAndLink(ParseRoot);

  Lex := TLexer.Create(FileNameToParse);
  Lex.ReadLine();

  { #done : how to re-initialize ParseRoot and New Node in case of Parse re-run ? }
  try
    Lex.Advance();
    statements();
  except
    on E: Exception do
    begin
      { #done : how to print partial parse tree when error ? }
      { #done : how to free memory in case of exception ? }
      writeln();
      writeln(E.message);
      PrintParseTree(ParseRoot, 0);
      FreeAndNil(Lex);
      Exit;
    end;
  end;
  PrintParseTree(ParseRoot, 0);
  WriteLn();
  WriteLn('Parsing finished with OK result');
  FreeAndNil(Lex);

end;

procedure TParser.statements();
var
  ThisNode: TParseNode;

begin
  ThisNode := NewNode;
  ThisNode.DisplayText := 'statements';

  while Lex.Lookahead.Name <> LINE_END do
  begin
    CreateNewNodeAndLink(ThisNode);
    expr();
    //Write(' = ' + FloatToStr(expr()));
    CreateNewNodeAndLink(ThisNode);
    NewNode.DisplayText := Lex.Lookahead.Lexeme;
    Lex.Match(SEMICOLON);
    Writeln(';');
  end;
  writeln('No more statements.');

end;

procedure TParser.expr;

var
  ThisNode: TParseNode;

begin
  ThisNode := NewNode;
  ThisNode.DisplayText := 'expr';
  CreateNewNodeAndLink(ThisNode);
  term();
  CreateNewNodeAndLink(ThisNode);
  expr_rest();
end;

procedure TParser.term;

var
  ThisNode: TParseNode;

begin
  ThisNode := NewNode;
  ThisNode.DisplayText := 'term';
  CreateNewNodeAndLink(ThisNode);
  factor();
  CreateNewNodeAndLink(ThisNode);
  term_rest();
end;

procedure TParser.expr_rest;
var
  ThisNode: TParseNode;
begin
  ThisNode := NewNode;
  ThisNode.DisplayText := 'expr_rest';

  case Lex.Lookahead.Name of
    PLUS: begin
      CreateNewNodeAndLink(ThisNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(PLUS);
      CreateNewNodeAndLink(ThisNode);
      term();
      CreateNewNodeAndLink(ThisNode);
      expr_rest();
      Write('+'); //postfix semantic action
    end;
    MINUS: begin
      CreateNewNodeAndLink(ThisNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(MINUS);
      CreateNewNodeAndLink(ThisNode);
      term();
      CreateNewNodeAndLink(ThisNode);
      expr_rest();
      Write('-');     //postfix semantic action
    end;

  end;
end;

procedure TParser.term_rest;
var
  ThisNode: TParseNode;
begin
  ThisNode := NewNode;
  ThisNode.DisplayText := 'term_rest';
  case Lex.Lookahead.Name of
    MULTIPLY: begin
      CreateNewNodeAndLink(ThisNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(MULTIPLY);
      CreateNewNodeAndLink(ThisNode);
      factor();
      CreateNewNodeAndLink(ThisNode);
      term_rest();
      Write('*');    //postfix semantic action
    end;
    DIVIDE: begin
      CreateNewNodeAndLink(ThisNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(DIVIDE);
      CreateNewNodeAndLink(ThisNode);
      factor();
      CreateNewNodeAndLink(ThisNode);
      term_rest();
      Write('/');        //postfix semantic action
    end;

  end;
end;

procedure TParser.factor;
var
  ThisNode: TParseNode;
begin
  ThisNode := NewNode;
  ThisNode.DisplayText := 'factor';
  case Lex.Lookahead.Name of
    NUMBER: begin
      CreateNewNodeAndLink(ThisNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Write(' ' + Lex.Lookahead.lexeme + ' ');
      Lex.Match(NUMBER);
    end;
    IDENTIFIER: begin
      CreateNewNodeAndLink(ThisNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Write(' ' + Lex.Lookahead.lexeme + ' ');
      Lex.Match(IDENTIFIER);
    end;
    LEFT_PARENS: begin
      CreateNewNodeAndLink(ThisNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(LEFT_PARENS);
      CreateNewNodeAndLink(ThisNode);
      expr();
      CreateNewNodeAndLink(ThisNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(RIGHT_PARENS);
    end;
    else
    begin
      raise Exception.Create('Syntax error in : ' +
        IntToStr(Lex.CurrentLineNumber) + ',' + IntToStr(Lex.CharPosition) +
        LineEnding + 'Num or Identifier or Left parens expected' +
        LineEnding + Lex.CurrentLine);
    end;
  end;
end;

{ TParseNode }

constructor TParseNode.Create;
begin
  ChildrenList := TList.Create;
end;

destructor TParseNode.Destroy;
begin
  FreeAndNil(ChildrenList);
  inherited Destroy;
end;


procedure TParseNode.Link(Node: TParseNode);
begin
  ChildrenList.add(Node);
end;



end.
