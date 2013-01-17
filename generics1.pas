//Copyright (c) 2012 by Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of MagicLibrary.
//
//MagicLibrary is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//MagicLibrary is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with MagicLibrary.  If not, see <http://www.gnu.org/licenses/>.

unit generics1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,

  Persists1, TextIO1;

type

  { TPersistsList }
  TListSortCompareObj = function (Item1, Item2: Pointer): Integer of object;

  generic TPersistsList<T> = class(TPersists)
  private
    fCount : Integer;
    fList : array of T;
    fSorted : Boolean;
    fNextID : Integer;

    procedure CheckCount( Value : Integer );
    function GetItemById( aId : Integer): T;
    procedure PutItemById( aId : Integer; AValue: T);

    procedure SetCapacity( Value : Integer );
    function  GetCapacity : Integer;
  protected
    procedure PutItem( Index : Integer; Item : T );
    function  GetItem( Index : Integer ) : T;
    function  MakeItem( ItemName : String ) : T;
    procedure DeleteItems;
  public
    constructor  Create( aParent : TPersists = nil ); override;
    destructor   Destroy; override;
    procedure    UNMODIFY; override;
    function     Add( Item : T; NoSetID : Boolean = False) : Integer;
    procedure    Clear; virtual;
    procedure    Delete( Index : Integer );
    procedure    DeleteItem( Item : T );
    procedure    Exchange( Index1, Index2: Integer );
    procedure    Expand;
    function     Extract( Item : T) : T;
    function     First : T;
    function     GetByID( aID : Integer ) : Integer;
    function     IndexOf( aName : String ) : Integer;
    function     IndexOfItem( Item : T ) : Integer;
    procedure    Insert( Index : Integer; Item : T );
    function     Last : T;
    procedure    Move( CurIndex, NewIndex :Integer );
    function     Remove( Item : T ) : Integer;
    procedure    Sort(Compare: TListSortCompare); overload;
    procedure    Sort(Compare: TListSortCompareObj); overload;

    procedure    Save( TextIO : TTextIO ); override;
    procedure    Read( TextIO : TTextIO; Version : Integer  ); override;
    procedure    Show( Memo : TMemo ); override;

    //procedure    Assign( Source : TPersists ); override;
    //procedure    AssignTo( Dest : TPersists ); override;

    property     Capacity : Integer read GetCapacity write SetCapacity;
    property     Count    : Integer read fCount;
    property     Items[I : Integer] : T read GetItem write PutItem; default;
    property     ItemById[ aId : Integer] : T read GetItemById write PutItemById;
    property     Sorted : Boolean read fSorted;
  end;

  { TStack }

  generic TStack<T> = class
    private
      SP : Integer;
      Stack : array of T;
    public
      constructor Create;  virtual;
      destructor  Destroy; virtual; // Produces warning if specialization is subclassed

      procedure Push( Value : T ); virtual;
      function  Pop : T; virtual;
  end;

implementation

uses
  ObjectFactory1;

{ TStack }

constructor TStack.Create;
const
  InitialListSize = 4;
var
  I : Integer;
begin
  SP := -1;
  SetLength(Stack,InitialListSize);
  for I := 0 to pred(InitialListSize) do
    if Stack[i] <> 0 then
      raise EListError.Create('non 0 in creation of Stack');
end;

destructor TStack.Destroy;
begin
  SetLength(Stack,0);
end;

procedure TStack.Push(Value: T);
begin
  Inc(SP);
  if SP >= Length(Stack) then
    SetLength(Stack,Length(Stack)*2);
  Stack[SP] := Value;
end;

function TStack.Pop: T;
begin
  if SP < 0 then
    raise EListError.Create('Stack Underflow');
  Result := Stack[SP];
  Dec(SP);
end;

{ TPersistsList }

procedure TPersistsList.CheckCount(Value: Integer);
begin
  if (Value < 0) or (Value >= fCount) then
    raise EListError.Create('Index out of bounds: 0 >= ' + IntToStr(Value) + ' < ' + IntToStr(fCount));
end;

function TPersistsList.GetItemById( aId : Integer): T;
begin
  Result := Items[GetById(aId)];
end;

procedure TPersistsList.PutItemById( aId : Integer; AValue: T);
begin
  Items[GetById(aId)] := AValue;
end;

procedure TPersistsList.SetCapacity(Value: Integer);
begin
  SetLength(fList,Value);
end;

function TPersistsList.GetCapacity: Integer;
begin
  Result := Length(fList);
end;

procedure TPersistsList.PutItem(Index: Integer; Item: T);
begin
  CheckCount( Index );
  fList[Index] := Item;
  Modify;
end;

function TPersistsList.GetItem(Index: Integer): T;
begin
  CheckCount(Index);
  Result := fList[Index];
end;

constructor TPersistsList.Create(aParent: TPersists);
const
  InitialListSize = 4;
var
  I : Integer;
begin
  inherited;
  fSorted := False;
  fNextID := 0;
  fName := 'List of ' + T.ClassName;

  SetLength(fList,InitialListSize);
  //for I := 0 to pred(InitialListSize) do
  //  if fList[i] <> nil then
  //    raise EListError.Create('non nil in create');
end;

destructor TPersistsList.Destroy;
begin
  { DONE 3 -oDon Z -cPossible memory leak; : Free the contents of the list before destroying it. }
  DeleteItems;
  SetLength(fList,0);
  inherited Destroy;
end;

procedure TPersistsList.UNMODIFY;
var
  I : Integer;
begin
  inherited UNMODIFY;
  for I := 0 to pred( Count ) do
    fList[I].UNMODIFY;
end;

function TPersistsList.Add(Item: T; NoSetID : Boolean ): Integer;
begin
  if (fCount >= Capacity) or (Capacity = 0) then
    Expand;
  if not NoSetID then
    begin
      Inc(fNextID);
      Item.ID := fNextID;
    end;
  Item.Parent := Self; // DRZ 2013-01-01
  fList[fCount] := Item;
  Inc(fCount);
//  Result := pred(fCount);
  Result := Item.ID;
  fSorted := False;
  Modify;
end;

//procedure TPersistsList.Assign(Source: T);
//begin
//{ TODO 1 -oDon Ziesig -cDevelopment : Implement this }
//  raise Exception.Create('Assign ' + Source.ClassName + ' NOT IMPLEMENTED');
//end;
//
//procedure TPersistsList.AssignTo(Dest: T);
//begin
//  { TODO 1 -oDon Ziesig -cDevelopment : Implement this }
//  raise Exception.Create('AssignTo ' + Dest.ClassName + ' NOT IMPLEMENTED');
//end;
//
procedure TPersistsList.Clear;
begin
  { DONE 3 -oDon Z -cPossible memory leak : Free the contents of the list before clearing it. }
  DeleteItems;
  SetLength( fList, 0);
  fCount := 0;
  fSorted := False;
end;

procedure TPersistsList.Delete(Index: Integer);
var
  I : Integer;
begin
  CheckCount(Index);
  fList[Index].Free;
  for I := succ(Index) to pred(fCount) do
    fList[pred(I)] := fList[I];
  Dec(fCount);
  Modify;
end;

procedure TPersistsList.DeleteItem(Item: T);
var
  I : Integer;
begin
  for I := 0 to pred(fCount) do
    if Items[I] = Item then
      begin
        Delete(I);
        Modify;
        break;
      end;
end;

procedure TPersistsList.DeleteItems;
var
  I : Integer;
begin
  for I := 0 to pred(fCount) do
    fList[I].Free;
end;

procedure TPersistsList.Exchange(Index1, Index2: Integer);
var
  Temp : T;
begin
  Temp := Items[Index1];
  Items[Index1] := Items[Index2];
  Items[Index2] := Temp;
  fSorted := False;
  Modify;
end;

procedure TPersistsList.Expand;
begin
  Capacity := Capacity * 2 +1;
end;

function TPersistsList.Extract(Item: T): T;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to pred(fCount) do
    if Items[I] = Item then
      begin
        Result := Items[I];
        Delete(I);
        Modify;
        break;
      end;
end;

function TPersistsList.First: T;
var
  I : Integer;
begin
  Result := Items[0];
  //Result := nil;
  //for I := 0 to pred(fCount) do
  //  if Items[I] <> nil then
  //    begin
  //      Result := Items[I];
  //      break;
  //    end;
end;

function TPersistsList.GetByID(aID: Integer): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to pred(fCount) do
    if Items[I].ID = aID then
      begin
        Result := I;
        break;
      end;
end;

function TPersistsList.IndexOfItem(Item: T): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to pred(fCount) do
    if Items[I] = Item then
      begin
        Result := I;
        break;
      end;
end;

function TPersistsList.IndexOf(aName: String): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to pred(fCount) do
    if Items[I].Name = aName then
      begin
        Result := I;
        break;
      end;
end;

procedure TPersistsList.Insert(Index: Integer; Item: T);
var
  I : Integer;
begin
  inc(fCount);
  if fCount >= Capacity then
    Expand;
  CheckCount( Index );
  for I := fCount-2 downto Index do
    Items[I+1] := Items[I];
  Inc(fNextID);
  Item.ID := fNextID;
  Items[Index] := Item;
  fSorted := False;
  Modify;
end;

function TPersistsList.Last: T;
var
  I : Integer;
begin
  Result := Items[pred(fCount)];
  //Result := nil;
  //for I := pred(fCount) downto 0 do
  //  if Items[I] <> nil then
  //    begin
  //      Result := Items[I];
  //      break;
  //    end;
end;

procedure TPersistsList.Save(TextIO: TTextIO);
const
  CurrentVersion = 1;
var
  I : Integer;
begin
  SaveHeader( TextIO, CurrentVersion);
  TextIO.Writeln(fNextID);
  TextIO.Writeln(Count);
  for I := 0 to pred(Count) do
    TPersists(fList[I]).Save( TextIO );
  SaveTrailer( TextIO );
end;

procedure TPersistsList.Read(TextIO: TTextIO; Version : Integer );
var
  C : Integer;
  I : Integer;
  O : TPersists;
begin
  C := 0;
  Clear;
  TextIO.ReadLn(fNextID);
  if Version >= 1 then
    begin
      TextIO.Readln(C);
      for I := 0 to pred(C) do
        begin
          O := Load( TextIO );
          O.Parent := Self as TPersists; // DRZ 2013-01-01
          Add( T(O), True ); // Add the object but don't update its ID field.
        end;
    end;
end;

procedure TPersistsList.Show(Memo : TMemo);
var
  I : Integer;
begin
  Memo.Lines.Add( 'Count => ' + IntToStr( Count ) );
  for I := 0 to pred( Count ) do
    TPersists(fList[I]).Show( Memo );
end;

//procedure TPersistsList.Assign(Source: TPersists);
//begin
//  inherited Assign(Source);
//end;
//
//procedure TPersistsList.AssignTo(Dest: TPersists);
//begin
//  inherited AssignTo(Dest);
//end;


function TPersistsList.MakeItem(ItemName: String): T;
var
  Nam  : String;
  Len : Integer;
begin
  // Make sure the ItemName is of the form <Name>
  if (ItemName[1] <> '<') or (ItemName[Length(ItemName)] <> '>') then
    raise Exception.Create('Invalid Item Name ['+ItemName+']');
  Len := Length( ItemName );
  Nam := Copy( ItemName, 2, Len - 2 );
  Result := T(ObjectFactory.MakeObject( Nam ).Create);
end;


procedure TPersistsList.Move(CurIndex, NewIndex: Integer);
var
  P : T;
begin
  P := Items[CurIndex];
  Delete(CurIndex);
  Insert(NewIndex,P);
  fSorted := False;
  Modify;
end;

function TPersistsList.Remove(Item: T): Integer;
var
  I : Integer;
begin
  Result := -1;
  I := IndexOfItem(Item);
  if I >= 0 then
    begin
      Delete(I);
      Result := I;
      Modify;
    end;

end;

procedure TPersistsList.Sort(Compare: TListSortCompare);
var
  P : T;
  I : Integer;
  C : Boolean;
begin
  { TODO 3 -oDon Z -cEfficiency : Implement a better (faster) sort here. }
  C := true;
  while C do
    begin
      C := False;
      for I := 1 to pred(fCount) do
        begin
          if Compare(fList[I-1],fList[I]) < 0 then
            begin
              P := fList[I-1];
              fList[I-1] := fList[I];
              fList[I] := P;
              C := True;
              Modify;
            end;
        end;
    end;
  fSorted := true;
end;

procedure TPersistsList.Sort(Compare: TListSortCompareObj);
var
  P : T;
  I : Integer;
  C : Boolean;
begin
  { TODO 3 -oDon Z -cEfficiency : Implement a better (faster) sort here. }
  C := true;
  while C do
    begin
      C := False;
      for I := 1 to pred(fCount) do
        begin
          if Compare(fList[I-1],fList[I]) < 0 then
            begin
              P := fList[I-1];
              fList[I-1] := fList[I];
              fList[I] := P;
              C := True;
              Modify;
            end;
        end;
    end;
  fSorted := true;
end;


end.

