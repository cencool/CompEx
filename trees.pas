unit Trees;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, Contnrs;

type
  int = integer;

  TTreeNode = record
    Value: int;
    left_child: ^TTreeNode;
    right_child: ^TTreeNode;
  end;

  PTreeNode = ^TTreeNode;


function BuildTreeBF(input_data: array of integer): PTreeNode;
procedure FreeTreeBF(root: PTreeNode);
procedure PrintTreeBF(root: PTreeNode);

implementation

function BuildTreeBF(input_data: array of integer): PTreeNode;
var
  in_index: int = 0;
  PNode: PTreeNode;
  current_parent: PTreeNode;
  queue: TQueue;

begin
  if (length(input_data) = 0) then
  begin
    writeln('No input data');
    exit(nil);
  end;
  WriteLn('Tree builder starts!');
  Writeln('Input data length: ', length(input_data));
  { create root node }
  Writeln('Processing: ', input_data[in_index]);
  new(PNode);
  PNode^.left_child := nil;
  PNode^.right_child := nil;
  PNode^.Value := input_data[in_index];
  Result := PNode;
  Inc(in_index);
  queue := TQueue.Create();
  queue.push(PNode);
  current_parent := queue.pop;

  while (in_index < length(input_data)) do
  begin
    Writeln('Processing: ', input_data[in_index]);
    new(PNode);
    PNode^.left_child := nil;
    PNode^.right_child := nil;
    PNode^.Value := input_data[in_index];
    Inc(in_index);

    if (current_parent^.left_child = nil) then
    begin
      current_parent^.left_child := PNode;
      queue.push(PNode);
    end
    else
    begin
      current_parent^.right_child := PNode;
      queue.push(PNode);
      current_parent := queue.pop;
    end;
  end;
  FreeAndNil(queue);

end;

procedure PrintTreeBF(root: PTreeNode);
var
  currentRow, nextRow: TQueue;
  currentNode: PTreeNode;
begin
  if (root = nil) then
  begin
    writeln('Nothing to print');
    exit;
  end;
  writeln('Printing tree..');
  currentRow := TQueue.Create;
  nextRow := TQueue.Create;
  nextRow.push(root);
  repeat
    while (nextRow.Count > 0) do
      currentRow.push(nextRow.pop());
    while (currentRow.Count > 0) do
    begin
      currentNode := currentRow.pop();
      Write(currentNode^.Value, ' ,');
      if (currentNode^.left_child <> nil) then
        nextRow.push(currentNode^.left_child);
      if (currentNode^.right_child <> nil) then
        nextRow.push(currentNode^.right_child);
    end;
    writeln();

  until (nextRow.Count <= 0);
  FreeAndNil(currentRow);
  FreeAndNil(nextRow);

end;

procedure FreeTreeBF(root: PTreeNode);
var
  currentRow, nextRow: TQueue;
  currentNode: PTreeNode;
begin
  if (root = nil) then
  begin
    writeln('Nothing to free');
    exit;
  end;
  writeln('Deleting tree..');
  currentRow := TQueue.Create;
  nextRow := TQueue.Create;
  nextRow.push(root);
  repeat
    while (nextRow.Count > 0) do
      currentRow.push(nextRow.pop());
    while (currentRow.Count > 0) do
    begin
      currentNode := currentRow.pop();
      Write(currentNode^.Value, ' ,');
      if (currentNode^.left_child <> nil) then
        nextRow.push(currentNode^.left_child);
      if (currentNode^.right_child <> nil) then
        nextRow.push(currentNode^.right_child);
      dispose(currentNode);
    end;
    writeln();

  until (nextRow.Count <= 0);
  FreeAndNil(currentRow);
  FreeAndNil(nextRow);
end;

end.


