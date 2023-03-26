unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,Lexer;

type

  { TParseNode }

  TParseNode = class
    Name: string;
    DisplayText: string;
    ChildrenList: TList;
    constructor Create();
    constructor CreateMod (Node: TParseNode);
  end;
  ParseNodePtr = ^TParseNode;

var
  SharedNode: TParseNode;
  Lex : TLexer;
  src_lines: TStringList;

procedure parse(fileName: string);
procedure statements();
function expr(): single;
function term(): single;
function factor(): single;
function expr_rest(semi: single): single;
function term_rest(semi: single): single;
function CreateParseNode(var NodePtr: ParseNodePtr): ParseNodePtr;


implementation

function CreateParseNode(var NodePtr: ParseNodePtr): ParseNodePtr;
begin
  new(NodePtr);
  NodePtr^.ChildrenList := TList.Create();
  NodePtr^.DisplayText := '';
  Result := NodePtr;
end;

procedure PrintParseTree(node: TParseNode; space_count: word);
var
  children_count: integer = 0;
  i: integer = 0;
begin
  children_count := node.ChildrenList.Count;
  for i := 1 to space_count do
  begin
    Write(' ');
  end;
  writeln(node.DisplayText);
  for i := 0 to (children_count - 1) do
  begin
    PrintParseTree(TParseNode(node.ChildrenList.Items[i]), space_count + 1);
  end;
end;

procedure parse(fileName: string);
var
  ParseTree: TParseNode;


begin
  src_lines := TStringList.Create;
  src_lines.LoadFromFile(fileName);
  Lex := TLexer.Create;
  Lex.current_line_number := 0;
  Lex.lookahead:= TToken.Create();
  Lex.read_line(src_lines);

  ParseTree := TParseNode.Create();
  SharedNode:= TParseNode.Create();
  ParseTree.ChildrenList.add(SharedNode);

  try
    Lex.advance();
    SharedNode.DisplayText := 'statements';
    statements();
  except
    on E: Exception do
    begin
      PrintParseTree(ParseTree, 0);
      writeln();
      writeln(E.message);
      Exit;
    end;
  end;
  PrintParseTree(ParseTree, 0);
  writeln();

  WriteLn('Parsing finished with OK result');
end;

procedure statements();
var
  StmtNode: TParseNode;
begin
  StmtNode := SharedNode;
  while Lex.lookahead.Name <> LINE_END do

  begin
    SharedNode := TParseNode.Create();
    StmtNode.ChildrenList.add(SharedNode);
    SharedNode.DisplayText := 'expr';

    Write(' = ' + FloatToStr(expr()));
    SharedNode:= TParseNode.Create();
    StmtNode.ChildrenList.add(SharedNode);
    SharedNode.DisplayText := 'SEMICOLON';
    Lex.match(SEMICOLON);
    Writeln(';');
  end;
  writeln();
  Exit();
end;

function expr(): single;
var
  MyNode: TParseNode;
  i: single;
begin
  MyNode := SharedNode;
  SharedNode:= TParseNode.Create();
  MyNode.ChildrenList.add(SharedNode);
  SharedNode.DisplayText := 'term';
  i := term();
  SharedNode:= TParseNode.Create();
  MyNode.ChildrenList.add(SharedNode);
  SharedNode.DisplayText := 'expr_rest';
  Result := expr_rest(i);
end;

function term(): single;
var
  MyNode: TParseNode;
  i: single;
begin
  MyNode := SharedNode;
  SharedNode:= TParseNode.Create();
  MyNode.ChildrenList.add(SharedNode);
  SharedNode.DisplayText := 'factor';
  i := factor();
  SharedNode:= TParseNode.Create();
  MyNode.ChildrenList.add(SharedNode);
  SharedNode.DisplayText := 'term_rest';
  Result := term_rest(i);
end;

function expr_rest(semi: single): single;
var
  MyNode: TParseNode;
  i: single;
begin
  case Lex.lookahead.Name of
    PLUS: begin
      MyNode := SharedNode;
      SharedNode:= TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'PLUS';
      Lex.match(PLUS);
      SharedNode:= TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'term';
      i := semi + term();
      SharedNode:= TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'expr_rest';
      Result := expr_rest(i);
      Write('+'); //postfix semantic action
    end;
    MINUS: begin
      MyNode := SharedNode;
      SharedNode:= TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'MINUS';
      Lex.match(MINUS);
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'term';
      i := semi - term();
      SharedNode:= TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'expr_rest';
      Result := expr_rest(i);
      Write('-');     //postfix semantic action
    end;
    else
      Result := semi;
  end;
end;

function term_rest(semi: single): single;
var
  MyNode: TParseNode;
  i: single;
begin
  case Lex.lookahead.Name of
    MULTIPLY: begin
      MyNode := SharedNode;
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'MULTIPLY';
      Lex.match(MULTIPLY);
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'factor';
      i := semi * factor();
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'term_rest';
      Result := term_rest(i);
      Write('*');    //postfix semantic action
    end;
    DIVIDE: begin
      MyNode := SharedNode;
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'DIVIDE';
      Lex.match(DIVIDE);
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'factor';
      i := semi / factor();
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'term_rest';
      Result := term_rest(i);
      Write('/');        //postfix semantic action
    end;
    else
      Result := semi;
  end;
end;

function factor(): single;
var
  MyNode: TParseNode;
begin
  case Lex.lookahead.Name of
    NUMBER: begin
      MyNode := SharedNode;
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'NUMBER';
      Result := (strtofloat(Lex.lookahead.lexeme));
      Write(' '+Lex.lookahead.lexeme + ' ');

      Lex.match(NUMBER);

    end;
    IDENTIFIER: begin
      MyNode := SharedNode;
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'IDENTIFIER';
      Write(' '+Lex.lookahead.lexeme + ' ');

      Lex.match(IDENTIFIER);
      Result := 1.0;
    end;
    LEFT_PARENS: begin
      MyNode := SharedNode;
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'LEFT_PARENS';
      Lex.match(LEFT_PARENS);
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'expr';
      Result := expr();
      SharedNode := TParseNode.Create();
      MyNode.ChildrenList.add(SharedNode);
      SharedNode.DisplayText := 'RIGHT_PARENS';
      Lex.match(RIGHT_PARENS);
    end;
    else
    begin
      raise Exception.Create('Syntax error in : ' +
        IntToStr(Lex.current_line_number) + ',' + IntToStr(Lex.char_position) +
        LineEnding + 'Num or Identifier or Left parens expected' +
        LineEnding + Lex.current_line);
    end;
  end;
end;

{ TParseNode }

constructor TParseNode.Create;
begin
    ChildrenList:= TList.Create;
    DisplayText:='';
    Name:='';
end;

constructor TParseNode.CreateMod(Node: TParseNode);
begin
    ChildrenList:= TList.Create;
    DisplayText:='';
    Name:='';
    Node := self;
end;

end.
