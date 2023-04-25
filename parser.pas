(*
Grammar:                               Syntax nodes:
prg -> block prg                       prg.n = new PrgSeq()
      | block                          prg.n = new PrgSeq()
block ->   '{' decls statements '}'    block.n = statements.n
decls -> decl decls                    decls.n = nil
        | @                            decls.n = nil
decl -> TYPENAME ID ';'                decl.n = nil
statements -> block                    statements.n = block.n
            | expr; statements         statements.n = new Seq()
            | @                        statements.n = nil
expr -> term rest                      expr.n = new Expr(Term();Expr(); nil)
expr_rest -> + term rest               expr_rest.n = new Expr(Term();Expr(); Lookahead)
            | - term rest | @          expr_rest.n = new Expr(Term();Expr(); Lookahead)
term -> factor term_rest               term.n = new Term(Factor(); Term(); nil)
term_rest -> * factor term_rest        term_rest.n = new Term(Factor(); Term(); Lookahead)
           | / factor term_rest        term_rest.n = new Term(Factor(); Term(); Lookahead)
           | @                         term_rest.n = nil
factor -> ID                           factor.n = new Factor (Lookahead)
        | NUMBER                       factor.n = new Factor (Lookahead)
        | ( expr)                      factor.n = expr.n

Implemented:
Seq
Expr
Term
Factor
*)
{ #todo : finish grammar comment }

unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  Lexer,
  Contnrs;

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
    procedure Gen; virtual;
    destructor Destroy; override;
  end;

  { TExpr }

  TExpr = class(TSyntaxNode)
    FExpr: TSyntaxNode;
    FTerm: TSyntaxNode;
    FTokenTag: TTokenTag;
    FTokenLexeme: string;
    constructor Create(ATerm: TSyntaxNode; AExpr: TSyntaxNode; AOpToken: TToken = nil);
    procedure Gen; override;
  end;

  { TTermNode }

  TTerm = class(TSyntaxNode)
    FFactor: TSyntaxNode;
    FTerm: TSyntaxNode;
    FTokenTag: TTokenTag;
    FTokenLexeme: string;
    constructor Create(AFactor: TSyntaxNode; ATerm: TSyntaxNode; AOpToken: TToken = nil);
    procedure Gen; override;
  end;

  { TPrgSeq }

  TPrgSeq = class(TSyntaxNode)
    FPrgSeq: TList;
    constructor Create;
    procedure Link(ANodeToLink: TSyntaxNode);
    procedure Gen; override;
  end;

  { TSequence }

  TSeq = class(TSyntaxNode)
    FExprSeq: TList;
    constructor Create;
    procedure Link(ANodeToLink: TSyntaxNode);
    procedure Gen; override;
  end;

  { TFactor }

  TFactor = class(TSyntaxNode)
    FTokenTag: TTokenTag;
    FTokenLexeme: string;
    constructor Create(AToken: TToken);
    procedure Gen; override;
  end;

  { TSemicolon }

  TSemicolon = class(TSyntaxNode)
    procedure Gen; override;
  end;

  { TNodes }

  TNodes = class
    ParseNode: TParseNode;
    SyntaxNode: TSyntaxNode;
    constructor Create;
    constructor CreateAssign(var ParseVar: TParseNode; var SyntaxVar: TSyntaxNode);
    constructor CreateAssignParse(var ParseVar: TParseNode);
    destructor Destroy; override;
  end;

  TSymbol = class
    SymbolType: string;
    SymbolPosition: array [0..1] of integer;
  end;

  { TSymbolTable }

  TSymbolTable = class
    ParentTable: TSymbolTable;
    Symbols: TFPObjectHashTable;
    constructor Create(Parent: TSymbolTable);
    destructor Destroy; override;
    procedure Add(ALexeme: string; ASymbol: TSymbol);
    function GetType(ALexeme: string): string;
    procedure PrintSymbols;
  end;

  { TParser }

  TParser = class
  public
    NewNode: TParseNode;
    ParseRoot: TParseNode;
    SyntaxRoot: TSyntaxNode;
    SymbolTableCurrent: TSymbolTable;
    { #done : add freeing objects }

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
    procedure PrintParseTree(Node: TParseNode; SpaceCount : word = 0);
    procedure FreeParseTree(Node: TParseNode);
    function FindSymbol(AIdentifier: string): TSymbol;
  end;


implementation

uses
  SysUtils;

var
  Lex: TLexer;

{ TSemicolon }

procedure TSemicolon.Gen;
begin
  WriteLn(';');
end;

{ TPrgSeq }

constructor TPrgSeq.Create;
begin
  FPrgSeq := TList.Create;
end;

procedure TPrgSeq.Link(ANodeToLink: TSyntaxNode);
begin
  FPrgSeq.add(ANodeToLink);
end;

procedure TPrgSeq.Gen;
var
  i: integer;
begin
  for i := 0 to FPrgSeq.Count - 1 do
  begin
    TSyntaxNode(FPrgSeq.Items[i]).Gen;
  end;
end;

{ TFactor }

constructor TFactor.Create(AToken: TToken);
begin
  FTokenTag := AToken.Tag;
  FTokenLexeme := AToken.Lexeme;
end;

procedure TFactor.Gen;
begin
  Write(' ' + FTokenLexeme + ' ');
end;

{ TSequence }

constructor TSeq.Create;
begin
  FExprSeq := TList.Create;
end;

procedure TSeq.Link(ANodeToLink: TSyntaxNode);
begin
  FExprSeq.Add(ANodeToLink);
end;

procedure TSeq.Gen;
var
  i: integer;
begin
  for i := 0 to FExprSeq.Count - 1 do
  begin
    TSyntaxNode(FExprSeq.Items[i]).Gen;
  end;

end;

{ TExpr }

constructor TExpr.Create(ATerm: TSyntaxNode; AExpr: TSyntaxNode; AOpToken: TToken = nil);
begin
  if AOpToken <> nil then
  begin
    if (AOpToken.Tag = PLUS) or (AOPToken.Tag = MINUS) then
    begin
      FTokenTag := AOpToken.Tag;
      FTokenLexeme := AOpToken.Lexeme;
    end
    else
      raise  Exception.Create(AOpToken.Lexeme + ' token passed to Expr Syntax Node');
  end
  else
    FTokenTag := NONE;
  FTerm := ATerm;
  FExpr := AExpr;
  FreeAndNil(AOpToken);
end;

procedure TExpr.Gen;
begin
  if FTerm <> nil then FTerm.Gen;
  if FExpr <> nil then FExpr.Gen;
  if FTokenTag <> NONE then
    Write(' ' + FTokenLexeme + ' ');
end;

{ TTerm }

constructor TTerm.Create(AFactor: TSyntaxNode; ATerm: TSyntaxNode; AOpToken: TToken = nil);
begin
  if AOpToken <> nil then
  begin
    if (AOpToken.Tag = MULTIPLY) or (AOPToken.Tag = DIVIDE) then
    begin
      FTokenTag := AOpToken.Tag;
      FTokenLexeme := AOpToken.Lexeme;
    end
    else
      raise  Exception.Create(AOpToken.Lexeme + ' token passed to Term Syntax Node');
  end
  else
    FTokenTag := NONE;
  FFactor := AFactor;
  FTerm := ATerm;
  FreeAndNil(AOpToken);
end;

procedure TTerm.Gen;
begin
  if FFactor <> nil then FFactor.Gen;
  if FTerm <> nil then FTerm.Gen;
  if FTokenTag <> NONE then Write(' ' + FTokenLexeme + ' ');
end;


{ TSymbolTable }

constructor TSymbolTable.Create(Parent: TSymbolTable);
begin
  ParentTable := Parent;
  Symbols := TFPObjectHashTable.Create();
end;

destructor TSymbolTable.Destroy;
begin
  FreeAndNil(Symbols);
  inherited Destroy;
end;

procedure TSymbolTable.Add(ALexeme: string; ASymbol: TSymbol);
begin
  Symbols.Add(ALexeme, ASymbol);
end;

function TSymbolTable.GetType(ALexeme: string): string;
begin
  if Symbols.Items[ALexeme] <> nil then
    Result := TSymbol(Symbols.Items[ALexeme]).SymbolType
  else
    Result := '';
end;

procedure TableIterator(Item: TObject; const key: string; var Continue: boolean);
begin
  continue := True;
  Write(key, ' ', TSymbol(Item).SymbolType + LineEnding);
end;

procedure TSymbolTable.PrintSymbols;

begin
  Symbols.Iterate(@TableIterator);
end;

procedure TSyntaxNode.Gen;
begin

end;



destructor TSyntaxNode.Destroy;
begin
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
  inherited Destroy;
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

function TParser.FindSymbol(AIdentifier: string): TSymbol;
var
  Table: TSymbolTable;
begin
  Table := SymbolTableCurrent;
  repeat
    Result := TSymbol(Table.Symbols.Items[AIdentifier]);
    if Result <> nil then
      Exit();
    Table := Table.ParentTable;
  until Table = nil;
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
    SyntaxRoot := Nodes.SyntaxNode;
    FreeAndNil(Nodes);
  except
    on E: Exception do
    begin
      { #todo : how to print partial parse tree when error ? }
      { #done : how to free memory in case of exception ? }
      writeln();
      writeln(E.message);
      if Nodes <> nil then
        PrintParseTree(ParseRoot);

      FreeAndNil(Lex);
      Exit;
    end;
  end;
  //PrintParseTree(ParseRoot, 0);
  //SyntaxRoot.Gen;

  WriteLn();
  WriteLn('Parsing finished with OK result');
  WriteLn('Words table content:');
  Lex.Words.Iterate(@Lex.WordsIterator);
  WriteLn();
  FreeAndNil(Lex);

end;

function TParser.prg(): TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'Program';
  Result.SyntaxNode := TPrgSeq.Create;

  while Lex.Lookahead.Tag <> NONE do
  begin
    Nodes := block();
    ParseNode.Link(Nodes.ParseNode);
    TPrgSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
    FreeAndNil(Nodes);
    //writeln('Return from block to prg');
  end;
end;

function TParser.block: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  StoredParent: TSymbolTable;

begin

  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'block';

  case Lex.Lookahead.Tag of
    CURLY_LEFT: begin
      Lex.Match(CURLY_LEFT);
      // symbol table init
      StoredParent := SymbolTableCurrent;
      SymbolTableCurrent := TSymbolTable.Create(StoredParent);

      ParseNode.AddChildWithText('{');

      //writeln('Block start');
      Nodes := decls();

      ParseNode.Link(Nodes.ParseNode);
      FreeAndNil(Nodes);

      Nodes := statements();
      ParseNode.Link(Nodes.ParseNode);
      Result.SyntaxNode := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Lex.Match(CURLY_RIGHT);
      ParseNode.AddChildWithText('}');
      //SymbolTableCurrent.PrintSymbols;
      SymbolTableCurrent := StoredParent;
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
  Symbol: TSymbol;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'decl';

  HelperString := Lex.Lookahead.Lexeme;
  Write(Lex.Lookahead.Lexeme, ' ');

  Symbol := TSymbol.Create;
  Symbol.SymbolType := Lex.Lookahead.Lexeme;

  Lex.Match(TYPENAME);
  HelperString := HelperString + ' ' + Lex.Lookahead.Lexeme;

  Write(Lex.Lookahead.Lexeme, ' ');

  Symbol.SymbolPosition := Lex.Lookahead.LexemePosition[0];
  try
    SymbolTableCurrent.Add(Lex.Lookahead.Lexeme, Symbol);
  except
    on E: EDuplicate do writeln(LineEnding + E.message);
  end;

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
  Result.SyntaxNode := TSeq.Create;

  while (Lex.Lookahead.Tag <> CURLY_RIGHT) do
  begin
    case Lex.Lookahead.Tag of
      CURLY_LEFT: begin
        Nodes := block();
        ParseNode.Link(Nodes.ParseNode);
        TSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
        FreeAndNil(Nodes);
      end;
      else
      begin
        Nodes := expr();
        ParseNode.Link(Nodes.ParseNode);
        TSeq(Result.SyntaxNode).Link(Nodes.SyntaxNode);
        FreeAndNil(Nodes);

        Lex.Match(SEMICOLON);
        TSeq(Result.SyntaxNode).Link(TSemicolon.Create); // workaround to print ';' end of expression

        ParseNode.AddChildWithText(';');

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
  AExpr: TSyntaxNode;
  ATerm: TSyntaxNode;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'expr';

  Nodes := term();
  ParseNode.Link(Nodes.ParseNode);
  ATerm := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Nodes := expr_rest();
  ParseNode.Link(Nodes.ParseNode);
  AExpr := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Result.SyntaxNode := TExpr.Create(ATerm, AExpr);
end;

function TParser.term: TNodes;

var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  AFactor: TSyntaxNode;
  ATerm: TSyntaxNode;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'term';

  Nodes := factor();
  ParseNode.Link(Nodes.ParseNode);
  AFactor := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Nodes := term_rest();
  ParseNode.Link(Nodes.ParseNode);
  ATerm := Nodes.SyntaxNode;
  FreeAndNil(Nodes);

  Result.SyntaxNode := TTerm.Create(AFactor, ATerm);
end;

function TParser.expr_rest: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  ATerm: TSyntaxNode;
  AExpr: TSyntaxNode;
  AToken: TToken;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'expr_rest';

  case Lex.Lookahead.Tag of
    PLUS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);
      Lex.Match(PLUS);

      Nodes := term();
      ParseNode.Link(Nodes.ParseNode);
      ATerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Nodes := expr_rest();
      ParseNode.Link(Nodes.ParseNode);
      AExpr := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TExpr.Create(ATerm, AExpr, AToken);

      Write('+'); //postfix semantic action
    end;
    MINUS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);

      Lex.Match(MINUS);
      Nodes := term();
      ParseNode.Link(Nodes.ParseNode);
      ATerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Nodes := expr_rest();
      ParseNode.Link(Nodes.ParseNode);
      AExpr := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TExpr.Create(ATerm, AExpr, AToken);


      Write('-');     //postfix semantic action
    end;

  end;
end;

function TParser.term_rest: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  AFactor: TSyntaxNode;
  ATerm: TSyntaxNode;
  AToken: TToken;
begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'term_rest';
  case Lex.Lookahead.Tag of
    MULTIPLY: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);
      Lex.Match(MULTIPLY);

      Nodes := factor();
      ParseNode.Link(Nodes.ParseNode);
      AFactor := Nodes.SyntaxNode;
      FreeAndNil(Nodes);
      Nodes := term_rest();
      ParseNode.Link(Nodes.ParseNode);
      ATerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TTerm.Create(AFactor, ATerm, AToken);

      Write('*');    //postfix semantic action
    end;
    DIVIDE: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      AToken := TToken.Create(Lex.Lookahead.Tag, Lex.Lookahead.Lexeme);

      Lex.Match(DIVIDE);
      Nodes := factor();

      ParseNode.Link(Nodes.ParseNode);
      AFactor := Nodes.SyntaxNode;

      FreeAndNil(Nodes);
      Nodes := term_rest();
      ParseNode.Link(Nodes.ParseNode);
      ATerm := Nodes.SyntaxNode;
      FreeAndNil(Nodes);

      Result.SyntaxNode := TTerm.Create(AFactor, ATerm, AToken);

      Write('/');        //postfix semantic action
    end;

  end;
end;

procedure TParser.PrintParseTree(Node: TParseNode; SpaceCount: word = 0);
var
  ChildrenCount: integer = 0;
  i: integer = 0;
begin
  if Node <> nil then
  begin
    ChildrenCount := Node.ChildrenList.Count;
    for i := 1 to SpaceCount do
    begin
      Write(' ');
    end;
    writeln(Node.DisplayText);
    for i := 0 to (ChildrenCount - 1) do
    begin
      PrintParseTree(TParseNode(Node.ChildrenList.Items[i]), SpaceCount + 1);
    end;
  end;
end;

function TParser.factor: TNodes;
var
  ParseNode: TParseNode = nil;
  Nodes: TNodes;
  s: string;
  row: integer;
  col: integer;

begin
  Result := TNodes.CreateAssignParse(ParseNode);
  ParseNode.DisplayText := 'factor';

  case Lex.Lookahead.Tag of
    NUMBER: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Write(' ' + Lex.Lookahead.Lexeme + ' ');
      Result.SyntaxNode := TFactor.Create(Lex.Lookahead);
      Lex.Match(NUMBER);
    end;
    IDENTIFIER: begin
      { #done : add searching whole symbol table chain }
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      row := Lex.Lookahead.LexemePosition[0][0];
      col := Lex.Lookahead.LexemePosition[0][1];
      if FindSymbol(Lex.Lookahead.Lexeme) <> nil then
      begin
        s := FindSymbol(Lex.Lookahead.Lexeme).SymbolType;
        Write(' ' + Lex.Lookahead.lexeme + ':' + s);
      end
      else
        raise Exception.Create('(' + IntToStr(row) + ',' + IntToStr(col) +
          ')' + ' Identifier ' + Lex.Lookahead.Lexeme + ' is not declared');
      Result.SyntaxNode := TFactor.Create(Lex.Lookahead);
      Lex.Match(IDENTIFIER);
    end;
    LEFT_PARENS: begin
      ParseNode.AddChildWithText(Lex.Lookahead.Lexeme);
      Lex.Match(LEFT_PARENS);
      Nodes := expr();
      ParseNode.Link(Nodes.ParseNode);
      Result.SyntaxNode := Nodes.SyntaxNode;
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
