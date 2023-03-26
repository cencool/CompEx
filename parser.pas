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

  TParser = class
    procedure parse(FileNameToParse: string);
    function statements(): TNodes;
    function expr(): TNodes;
    function term(): TNodes;
    function factor(): TNodes;
    function expr_rest(): TNodes;
    function term_rest(): TNodes;
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

procedure TParser.parse(FileNameToParse: string);

var
  TreeRoot: TNodes;

begin
  Lex := TLexer.Create(FileNameToParse);
  Lex.ReadLine();

  try
    Lex.Advance();
    TreeRoot := statements();
  except
    on E: Exception do
    begin
      PrintParseTree(TreeRoot.ParseNode, 0);
      writeln();
      writeln(E.message);
      Exit;
    end;
  end;
  PrintParseTree(TreeRoot.ParseNode, 0);
  writeln();

end;

function TParser.statements(): TNodes;
var
  TempNode: TParseNode;

begin
  Result := TNodes.Create;
  Result.ParseNode.Name := 'statements';
  Result.ParseNode.DisplayText := 'statements';
  while Lex.Lookahead.Name <> LINE_END do
  begin
    Result.ParseNode.ChildrenList.Add(expr().ParseNode);
    //Write(' = ' + FloatToStr(expr()));
    TempNode := TParseNode.Create();
    writestr(TempNode.Name, Lex.Lookahead.Name);
    TempNode.DisplayText := Lex.Lookahead.Lexeme;
    Lex.Match(SEMICOLON);
    Result.ParseNode.ChildrenList.Add(TempNode);
    Writeln(';');
  end;
  writeln('No more statements.');
  WriteLn('Parsing finished with OK result');

end;

function TParser.expr(): TNodes;

begin
  Result := TNodes.Create;
  Result.ParseNode.Name := 'expr';
  Result.ParseNode.DisplayText := 'expr';
  Result.ParseNode.ChildrenList.Add(term().ParseNode);
  Result.ParseNode.ChildrenList.Add(expr_rest().ParseNode);
end;

function TParser.term(): TNodes;

begin
  Result := TNodes.Create;
  Result.ParseNode.Name := 'term';
  Result.ParseNode.DisplayText := 'term';
  Result.ParseNode.ChildrenList.Add(factor().ParseNode);
  Result.ParseNode.ChildrenList.Add(term_rest().ParseNode);
end;

function TParser.expr_rest(): TNodes;
var
  TempNode: TParseNode;
begin
  Result := TNodes.Create;
  Result.ParseNode.Name := 'expr_rest';
  Result.ParseNode.DisplayText := 'expr_rest';
  case Lex.Lookahead.Name of
    PLUS: begin
      TempNode := TParseNode.Create();
      writestr(TempNode.Name, Lex.Lookahead.Name);
      TempNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(PLUS);
      Result.ParseNode.ChildrenList.Add(TempNode);
      Result.ParseNode.ChildrenList.Add(term().ParseNode);
      Result.ParseNode.ChildrenList.Add(expr_rest().ParseNode);
      Write('+'); //postfix semantic action
    end;
    MINUS: begin
      TempNode := TParseNode.Create();
      writestr(TempNode.Name, Lex.Lookahead.Name);
      TempNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(MINUS);
      Result.ParseNode.ChildrenList.Add(TempNode);
      Result.ParseNode.ChildrenList.Add(term().ParseNode);
      Result.ParseNode.ChildrenList.Add(expr_rest().ParseNode);
      Write('-');     //postfix semantic action
    end;

  end;
end;

function TParser.term_rest(): TNodes;
var
  TempNode: TParseNode;
begin
  Result := TNodes.Create;
  Result.ParseNode.Name := 'term_rest';
  Result.ParseNode.DisplayText := 'term_rest';
  case Lex.Lookahead.Name of
    MULTIPLY: begin
      TempNode := TParseNode.Create();
      writestr(TempNode.Name, Lex.Lookahead.Name);
      TempNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(MULTIPLY);
      Result.ParseNode.ChildrenList.Add(TempNode);
      Result.ParseNode.ChildrenList.Add(factor().ParseNode);
      Result.ParseNode.ChildrenList.Add(term_rest().ParseNode);
      Write('*');    //postfix semantic action
    end;
    DIVIDE: begin
      TempNode := TParseNode.Create();
      writestr(TempNode.Name, Lex.Lookahead.Name);
      TempNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(DIVIDE);
      Result.ParseNode.ChildrenList.Add(TempNode);
      Result.ParseNode.ChildrenList.Add(factor().ParseNode);
      Result.ParseNode.ChildrenList.Add(term_rest().ParseNode);
      Write('/');        //postfix semantic action
    end;

  end;
end;

function TParser.factor(): TNodes;
var
  TempNode: TParseNode;
begin
  Result := TNodes.Create;
  Result.ParseNode.Name := 'factor';
  Result.ParseNode.DisplayText := 'factor';
  case Lex.Lookahead.Name of
    NUMBER: begin
      TempNode := TParseNode.Create();
      writestr(TempNode.Name, Lex.Lookahead.Name);
      TempNode.DisplayText := Lex.Lookahead.Lexeme;
      Write(' ' + Lex.Lookahead.lexeme + ' ');
      Lex.Match(NUMBER);
      Result.ParseNode.ChildrenList.Add(TempNode);

    end;
    IDENTIFIER: begin
      TempNode := TParseNode.Create();
      writestr(TempNode.Name, Lex.Lookahead.Name);
      TempNode.DisplayText := Lex.Lookahead.Lexeme;
      Write(' ' + Lex.Lookahead.lexeme + ' ');
      Lex.Match(IDENTIFIER);
      Result.ParseNode.ChildrenList.Add(TempNode);
    end;
    LEFT_PARENS: begin
      TempNode := TParseNode.Create();
      writestr(TempNode.Name, Lex.Lookahead.Name);
      TempNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(LEFT_PARENS);
      Result.ParseNode.ChildrenList.Add(TempNode);
      Result.ParseNode.ChildrenList.Add(expr().ParseNode);
      TempNode := TParseNode.Create();
      writestr(TempNode.Name, Lex.Lookahead.Name);
      TempNode.DisplayText := Lex.Lookahead.Lexeme;
      Lex.Match(RIGHT_PARENS);
      Result.ParseNode.ChildrenList.Add(TempNode);
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



end.
