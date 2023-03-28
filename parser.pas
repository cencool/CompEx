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

    constructor Create;
    procedure CreateNewNode;
    procedure parse(FileNameToParse: string);
    procedure statements();
    procedure expr();
    procedure term();
    procedure factor();
    procedure expr_rest();
    procedure term_rest();
    procedure PrintParseTree(Root: TParseNode; space_count: word);
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

procedure TParser.PrintParseTree(Root: TParseNode; space_count: word);
var
  ChildrenCount: integer = 0;
  i: integer = 0;
begin
  ChildrenCount := Root.ChildrenList.Count;
  for i := 1 to space_count do
  begin
    Write(' ');
  end;
  writeln(Root.DisplayText);
  for i := 0 to (ChildrenCount - 1) do
  begin
    PrintParseTree(TParseNode(Root.ChildrenList.Items[i]), space_count + 1);
  end;
end;

constructor TParser.Create;
begin
  NewNode := TParseNode.Create;
end;

procedure TParser.CreateNewNode;
begin
  NewNode := TParseNode.Create();
end;

procedure TParser.parse(FileNameToParse: string);

var
  ParseRoot: TParseNode;

begin
  Lex := TLexer.Create(FileNameToParse);
  Lex.ReadLine();
  ParseRoot := NewNode; { #todo : What display text for ParseRoot ? }
  CreateNewNode;
  ParseRoot.Link(NewNode);

  try
    Lex.Advance();
    statements();
  except
    on E: Exception do
    begin
      PrintParseTree(ParseRoot, 0);
      { #todo : how to print partial parse tree when error ? }
      writeln();
      writeln(E.message);
      Exit;
    end;
  end;
  PrintParseTree(ParseRoot, 0);
  writeln();
  WriteLn('Parsing finished with OK result');

end;

procedure TParser.statements();
var
  ThisNode: TParseNode;

begin
  ThisNode := NewNode;
  ThisNode.DisplayText := 'statements';

  while Lex.Lookahead.Name <> LINE_END do
  begin
    CreateNewNode;
    ThisNode.Link(NewNode);
    expr();
    //Write(' = ' + FloatToStr(expr()));
    CreateNewNode;
    ThisNode.Link(NewNode);
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
  CreateNewNode;
  ThisNode.Link(NewNode);
  term();
  CreateNewNode;
  ThisNode.Link(NewNode);
  expr_rest();
end;

procedure TParser.term;

var
  ThisNode: TParseNode;

begin
  ThisNode := NewNode;
  ThisNode.DisplayText := 'term';
  CreateNewNode;
  ThisNode.Link(NewNode);
  factor();
  CreateNewNode;
  ThisNode.Link(NewNode);
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
      CreateNewNode;
      ThisNode.Link(NewNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(PLUS);
      CreateNewNode;
      ThisNode.Link(NewNode);
      term();
      CreateNewNode;
      ThisNode.Link(NewNode);
      expr_rest();
      Write('+'); //postfix semantic action
    end;
    MINUS: begin
      CreateNewNode;
      ThisNode.Link(NewNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(MINUS);
      CreateNewNode;
      ThisNode.Link(NewNode);
      term();
      CreateNewNode;
      ThisNode.Link(NewNode);
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
      CreateNewNode;
      ThisNode.Link(NewNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(MULTIPLY);
      CreateNewNode;
      ThisNode.Link(NewNode);
      factor();
      CreateNewNode;
      ThisNode.Link(NewNode);
      term_rest();
      Write('*');    //postfix semantic action
    end;
    DIVIDE: begin
      CreateNewNode;
      ThisNode.Link(NewNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(DIVIDE);
      ThisNode.Link(NewNode);
      factor();
      CreateNewNode;
      ThisNode.Link(NewNode);
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
      CreateNewNode;
      ThisNode.Link(NewNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Write(' ' + Lex.Lookahead.lexeme + ' ');
      Lex.Match(NUMBER);
    end;
    IDENTIFIER: begin
      CreateNewNode;
      ThisNode.Link(NewNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Write(' ' + Lex.Lookahead.lexeme + ' ');
      Lex.Match(IDENTIFIER);
    end;
    LEFT_PARENS: begin
      CreateNewNode;
      ThisNode.Link(NewNode);
      NewNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(LEFT_PARENS);
      CreateNewNode;
      ThisNode.Link(NewNode);
      expr();
      CreateNewNode;
      ThisNode.Link(NewNode);
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
  DisplayText := '';
  Name := '';
end;

procedure TParseNode.Link(Node: TParseNode);
begin
  ChildrenList.add(Node);
end;



end.
