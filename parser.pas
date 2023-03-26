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
  end;

  TParser = class

  end;

var
  Lex : TLexer;

procedure parse(FileNameToParse: string);
procedure statements();
function expr(): single;
function term(): single;
function factor(): single;
function expr_rest(semi: single): single;
function term_rest(semi: single): single;


implementation

procedure PrintParseTree(node: TParseNode; space_count: word);
var
  ChildrenCount: integer = 0;
  i: integer = 0;
begin
  ChildrenCount := node.ChildrenList.Count;
  for i := 1 to space_count do
  begin
    Write(' ');
  end;
  writeln(node.DisplayText);
  for i := 0 to (ChildrenCount - 1) do
  begin
    PrintParseTree(TParseNode(node.ChildrenList.Items[i]), space_count + 1);
  end;
end;

procedure parse(FileNameToParse: string);
var
  ParseTree: TParseNode;


begin
  Lex := TLexer.Create(FileNameToParse);
  Lex.ReadLine();


  try
    Lex.Advance();
    statements();
  except
    on E: Exception do
    begin
      //PrintParseTree(ParseTree, 0);
      writeln();
      writeln(E.message);
      Exit;
    end;
  end;
  //PrintParseTree(ParseTree, 0);
  writeln();

  WriteLn('Parsing finished with OK result');
end;

procedure statements();

begin
  while Lex.Lookahead.Name <> LINE_END do

  begin

    Write(' = ' + FloatToStr(expr()));
    Lex.Match(SEMICOLON);
    Writeln(';');
  end;
  writeln();
  Exit();
end;

function expr(): single;
var
  i: single;
begin
  i := term();
  Result := expr_rest(i);
end;

function term(): single;
var
  i: single;
begin
  i := factor();
  Result := term_rest(i);
end;

function expr_rest(semi: single): single;
var
  i: single;
begin
  case Lex.Lookahead.Name of
    PLUS: begin
      Lex.Match(PLUS);
      i := semi + term();
      Result := expr_rest(i);
      Write('+'); //postfix semantic action
    end;
    MINUS: begin
      Lex.Match(MINUS);
      i := semi - term();
      Result := expr_rest(i);
      Write('-');     //postfix semantic action
    end;
    else
      Result := semi;
  end;
end;

function term_rest(semi: single): single;
var
  i: single;
begin
  case Lex.Lookahead.Name of
    MULTIPLY: begin
      Lex.Match(MULTIPLY);
      i := semi * factor();
      Result := term_rest(i);
      Write('*');    //postfix semantic action
    end;
    DIVIDE: begin
      Lex.Match(DIVIDE);
      i := semi / factor();
      Result := term_rest(i);
      Write('/');        //postfix semantic action
    end;
    else
      Result := semi;
  end;
end;

function factor(): single;
begin
  case Lex.Lookahead.Name of
    NUMBER: begin
      Result := (strtofloat(Lex.Lookahead.lexeme));
      Write(' '+Lex.Lookahead.lexeme + ' ');

      Lex.Match(NUMBER);

    end;
    IDENTIFIER: begin
      Write(' '+Lex.Lookahead.lexeme + ' ');

      Lex.Match(IDENTIFIER);
      Result := 1.0;
    end;
    LEFT_PARENS: begin
      Lex.Match(LEFT_PARENS);
      Result := expr();
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
    ChildrenList:= TList.Create;
    DisplayText:='';
    Name:='';
end;



end.
