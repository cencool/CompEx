(*
Grammar:
program ->   block
block ->   '{' decls statements '}'
decls -> decl decls | @
decl -> TYPENAME ID ;
statements -> sta;statements |  @
statement -> expr
*)
{ #todo : finish grammar coment }

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
    procedure AddChildWithText(TextToAdd: string);
  end;

  { TSyntaxNode }

  TSyntaxNode = class
    Name: string;
    DisplayText: string;
    Value: double;
    ChildrenList: TList;
    constructor Create;
    destructor Destroy; override;
  end;

  { TNodes }

  TNodes = class
    ParseNode: TParseNode;
    SyntaxNode: TSyntaxNode;
    constructor Create;
    constructor CreateAssign(var ParseVar: TParseNode; var SyntaxVar: TSyntaxNode);
    constructor CreateAssignParse(var ParseVar:TParseNode);
    destructor Destroy; override;
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
    function prg(): TNodes;
    function block(): TNodes;
    function decls(): TNodes;
    function decl(): TNodes;
    function statements(): TNodes;
    function expr(): TNodes;
    function term(): TNodes;
    function factor(): TNodes;
    function expr_rest(): TNodes;
    function term_rest(): TNodes;
    procedure PrintParseTree(Node: TParseNode; space_count: word);
    procedure FreeParseTree(Node: TParseNode);
  end;


implementation

uses
  SysUtils, Lexer;

var
  Lex: TLexer;

{ TSyntaxNode }

constructor TSyntaxNode.Create;
begin
  ChildrenList := TList.Create;
end;

destructor TSyntaxNode.Destroy;
begin
  FreeAndNil(ChildrenList);
  inherited Destroy;
end;

{ TNodes }

constructor TNodes.Create;
begin
  ParseNode := TParseNode.Create;
  SyntaxNode := TSyntaxNode.Create;
end;

constructor TNodes.CreateAssign(var ParseVar: TParseNode; var SyntaxVar: TSyntaxNode);
begin
  ParseNode := TParseNode.Create();
  SyntaxNode := TSyntaxNode.Create();
  ParseVar := ParseNode;
  SyntaxVar := SyntaxNode;
end;

constructor TNodes.CreateAssignParse(var ParseVar: TParseNode);
begin
  ParseNode := TParseNode.Create();
  ParseVar := ParseNode;
end;

destructor TNodes.Destroy;
begin
  //FreeAndNil(ParseNode);
  //FreeAndNil(SyntaxNode);
  inherited Destroy;
end;

procedure TParser.PrintParseTree(Node: TParseNode; space_count: word);
var
  ChildrenCount: integer = 0;
  i: integer = 0;
begin
  if Node <> nil then
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
var
  Nodes: TNodes = nil;

begin
  FreeParseTree(ParseRoot);


  Lex := TLexer.Create(FileNameToParse);
  Lex.ReadLine();

  { #done : how to re-initialize ParseRoot and New Node in case of Parse re-run ? }
  try
    Lex.Advance();
    Nodes := prg();
    ParseRoot := Nodes.ParseNode;
    FreeAndNil(Nodes);
  except
    on E: Exception do
    begin
      { #todo : how to print partial parse tree when error ? }
      { #done : how to free memory in case of exception ? }
      writeln();
      writeln(E.message);
      if Nodes <> nil then
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

function TParser.prg(): TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'Program';

  while Lex.Lookahead.Tag <> NONE do
  begin
    Nodes := block();
    ParseNode.Link(Nodes.ParseNode);
    FreeAndNil(Nodes);
    //writeln('Return from block to prg');
  end;
end;

function TParser.block: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'block';


  case Lex.Lookahead.Tag of
    CURLY_LEFT: begin
      Lex.Match(CURLY_LEFT);

      ParseNode.AddChildWithText('{');

      //writeln('Block start');
      Nodes := decls();

      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);

      Nodes := statements();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Lex.Match(CURLY_RIGHT);
      ParseNode.AddChildWithText('}');

      //writeln('Block end');
    end;
    else
    begin
      { #todo : correct char position report in case of syntax error }
      raise Exception.Create('Syntax error in : ' +
        IntToStr(Lex.CurrentLineNumber) + ',' + IntToStr(Lex.CharPosition) +
        LineEnding + 'Curly Brackets expected' + LineEnding + Lex.CurrentLine);
    end;
  end;

end;

function TParser.decls: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'decls';

  while Lex.Lookahead.Tag = TYPENAME do
  begin
    Nodes := decl();
    ParseNode.Link(Nodes.ParseNode);
    FreeAndNil(Nodes);
  end;
end;

function TParser.decl: TNodes;
var
  ParseNode: TParseNode = nil;
  HelperString: string;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'decl';

  HelperString := Lex.Lookahead.Lexeme;
  Write(Lex.Lookahead.Lexeme, ' ');
  Lex.Match(TYPENAME);
  HelperString := HelperString + ' ' + Lex.Lookahead.Lexeme;

  Write(Lex.Lookahead.Lexeme, ' ');

  Lex.Match(IDENTIFIER);
  Write(Lex.Lookahead.Lexeme + LineEnding);
  HelperString := HelperString + ' ' + Lex.Lookahead.Lexeme;

  Lex.Match(SEMICOLON);
  ParseNode.AddChildWithText(HelperString);

end;

function TParser.statements(): TNodes;
  { #todo : add distinction statement vs expression }
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'statements';

  while (Lex.Lookahead.Tag <> CURLY_RIGHT) do
  begin
    case Lex.Lookahead.Tag of
      CURLY_LEFT: begin
        Nodes := block();
        ParseNode.Link(Nodes.ParseNode);
        FreeAndNil(Nodes);
      end;
      else
      begin
        Nodes := expr();
        ParseNode.Link(Nodes.ParseNode);
        FreeAndNil(Nodes);

        ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);

        Lex.Match(SEMICOLON);
        Writeln(';');
      end;
    end;
    //writeln('Statements processed');

  end;

end;

function TParser.expr: TNodes;

var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'expr';

  Nodes := term();
  ParseNode.Link(Nodes.ParseNode);
  FreeAndNil(Nodes);

  Nodes := expr_rest();
  ParseNode.Link(Nodes.ParseNode);
  FreeAndNil(Nodes);
end;

function TParser.term: TNodes;

var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'term';

  Nodes := factor();
  ParseNode.Link(Nodes.ParseNode);
  FreeAndNil(Nodes);

  Nodes := term_rest();
  ParseNode.Link(Nodes.ParseNode);
  FreeAndNil(Nodes);
end;

function TParser.expr_rest: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'expr_rest';

  case Lex.Lookahead.Tag of
    PLUS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(PLUS);

      Nodes := term();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Nodes := expr_rest();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Write('+'); //postfix semantic action
    end;
    MINUS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(MINUS);
      Nodes := term();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Nodes := expr_rest();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Write('-');     //postfix semantic action
    end;

  end;
end;

function TParser.term_rest: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'term_rest';
  case Lex.Lookahead.Tag of
    MULTIPLY: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(MULTIPLY);

      Nodes := factor();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Nodes := term_rest();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Write('*');    //postfix semantic action
    end;
    DIVIDE: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(DIVIDE);
      Nodes := factor();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Nodes := term_rest();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      Write('/');        //postfix semantic action
    end;

  end;
end;

function TParser.factor: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'factor';

  case Lex.Lookahead.Tag of
    NUMBER: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Write(' ' + Lex.Lookahead.Lexeme + ' ');
      Lex.Match(NUMBER);
    end;
    IDENTIFIER: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Write(' ' + Lex.Lookahead.lexeme + ' ');
      Lex.Match(IDENTIFIER);
    end;
    LEFT_PARENS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(LEFT_PARENS);
      Nodes := expr();
      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(RIGHT_PARENS);
    end
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

procedure TParseNode.AddChildWithText(TextToAdd: string);
var
  Node: TParseNode;
begin
  Node := TParseNode.Create();
  Node.DisplayText := TextToAdd;
  Link(Node);
end;



end.
