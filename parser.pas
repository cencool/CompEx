unit Parser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8,Lexer;

type
  TParseNode = record
    display_text: string;
    ChildrenList: TList;
  end;
  ParseNodePtr = ^TParseNode;

var
  SharedNodePtr: ParseNodePtr;
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
  NodePtr^.display_text := '';
  Result := NodePtr;
end;

procedure PrintParseTree(node: ParseNodePtr; space_count: word);
var
  children_count: integer = 0;
  i: integer = 0;
begin
  children_count := node^.ChildrenList.Count;
  for i := 1 to space_count do
  begin
    Write(' ');
  end;
  writeln(node^.display_text);
  for i := 0 to (children_count - 1) do
  begin
    PrintParseTree(node^.ChildrenList.Items[i], space_count + 1);
  end;
end;

procedure parse(fileName: string);
var
  ParseTreePtr: ParseNodePtr;


begin
  src_lines := TStringList.Create;
  src_lines.LoadFromFile(fileName);
  current_line_number := 0;
  lookahead:= TToken.Create();
  read_line(src_lines);

  ParseTreePtr := CreateParseNode(SharedNodePtr);
  CreateParseNode(SharedNodePtr);
  ParseTreePtr^.ChildrenList.add(SharedNodePtr);

  try
    advance();
    SharedNodePtr^.display_text := 'statements';
    statements();
  except
    on E: Exception do
    begin
      PrintParseTree(ParseTreePtr, 0);
      writeln();
      writeln(E.message);
      Exit;
    end;
  end;
  PrintParseTree(ParseTreePtr, 0);
  writeln();

  WriteLn('Parsing finished with OK result');
end;

procedure statements();
var
  StmtNodePtr: ParseNodePtr;
begin
  StmtNodePtr := SharedNodePtr;
  while lookahead.Name <> LINE_END do

  begin
    StmtNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
    SharedNodePtr^.display_text := 'expr';

    Write(' = ' + FloatToStr(expr()));
    StmtNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
    SharedNodePtr^.display_text := 'SEMICOLON';
    match(SEMICOLON);
    Writeln(';');
  end;
  writeln();
  Exit();
end;

function expr(): single;
var
  MyNodePtr: ParseNodePtr;
  i: single;
begin
  MyNodePtr := SharedNodePtr;
  MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
  SharedNodePtr^.display_text := 'term';
  i := term();
  MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
  SharedNodePtr^.display_text := 'expr_rest';
  Result := expr_rest(i);
end;

function term(): single;
var
  MyNodePtr: ParseNodePtr;
  i: single;
begin
  MyNodePtr := SharedNodePtr;
  MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
  SharedNodePtr^.display_text := 'factor';
  i := factor();
  MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
  SharedNodePtr^.display_text := 'term_rest';
  Result := term_rest(i);
end;

function expr_rest(semi: single): single;
var
  MyNodePtr: ParseNodePtr;
  i: single;
begin
  case lookahead.Name of
    PLUS: begin
      MyNodePtr := SharedNodePtr;
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'PLUS';
      match(PLUS);
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'term';
      i := semi + term();
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'expr_rest';
      Result := expr_rest(i);
      Write('+'); //postfix semantic action
    end;
    MINUS: begin
      MyNodePtr := SharedNodePtr;
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'MINUS';
      match(MINUS);
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'term';
      i := semi - term();
      SharedNodePtr^.display_text := 'expr_rest';
      Result := expr_rest(i);
      Write('-');     //postfix semantic action
    end;
    else
      Result := semi;
  end;
end;

function term_rest(semi: single): single;
var
  MyNodePtr: ParseNodePtr;
  i: single;
begin
  case lookahead.Name of
    MULTIPLY: begin
      MyNodePtr := SharedNodePtr;
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'MULTIPLY';
      match(MULTIPLY);
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'factor';
      i := semi * factor();
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'term_rest';
      Result := term_rest(i);
      Write('*');    //postfix semantic action
    end;
    DIVIDE: begin
      MyNodePtr := SharedNodePtr;
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'DIVIDE';
      match(DIVIDE);
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'factor';
      i := semi / factor();
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'term_rest';
      Result := term_rest(i);
      Write('/');        //postfix semantic action
    end;
    else
      Result := semi;
  end;
end;

function factor(): single;
var
  MyNodePtr: ParseNodePtr;
begin
  case lookahead.Name of
    NUMBER: begin
      MyNodePtr := SharedNodePtr;
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'NUMBER';
      Result := (strtofloat(lookahead.lexeme));
      Write(' '+lookahead.lexeme + ' ');

      match(NUMBER);

    end;
    IDENTIFIER: begin
      MyNodePtr := SharedNodePtr;
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'IDENTIFIER';
      Write(' '+lookahead.lexeme + ' ');

      match(IDENTIFIER);
      Result := 1.0;
    end;
    LEFT_PARENS: begin
      MyNodePtr := SharedNodePtr;
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'LEFT_PARENS';
      match(LEFT_PARENS);
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'expr';
      Result := expr();
      MyNodePtr^.ChildrenList.add(CreateParseNode(SharedNodePtr));
      SharedNodePtr^.display_text := 'RIGHT_PARENS';
      match(RIGHT_PARENS);
    end;
    else
    begin
      raise Exception.Create('Syntax error in : ' +
        IntToStr(current_line_number) + ',' + IntToStr(char_position) +
        LineEnding + 'Num or Identifier or Left parens expected' +
        LineEnding + current_line);
    end;
  end;
end;

end.
