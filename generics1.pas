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

{.define DEBUG_INSERT}

unit generics1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls,

  Persists1, TextIO1;

type

  { closure which allows sorting and/or indexing }

  TListSortCompareObj = function (Left, Right: Pointer): Integer of object;
  TListSortCompare    = function (Left, Right: Pointer): Integer;

{==============================================================================}
{ TPersistsList }
{==============================================================================}

  generic TPersistsList<T> = class(TPersists)
  private
    fCount  : Integer;    // How many Items in the list (array);
    fList   : array of T; // The Items
    fSorted : Boolean;    // Is the list sorted.  USE CARE TO ENSURE THIS IS CORRECT
    fNextID : Integer;    // The ID of the next object to be added to the list.

    procedure    CheckCount( Value : Integer );
    function     GetItemById( aId : Integer): T;
    procedure    PutItemById( aId : Integer; AValue: T);

    procedure    SetCapacity( Value : Integer );
    function     GetCapacity : Integer;
  protected
    procedure    PutItem( Index : Integer; Item : T );
    function     GetItem( Index : Integer ) : T;
    function     MakeItem( ItemName : String ) : T; // Uses objectFactory to make a new T;
    procedure    DeleteItems;
    function     Add( Item : T; NoSetID : Boolean) : Integer; overload; // Needed to Add a just-read Item without overwriting its ID.
    procedure    Expand;
  public

    constructor  Create( aParent : TPersists = nil ); override;
    destructor   Destroy; override;
    procedure    UNMODIFY; override;
    function     Add( Item : T ) : Integer; overload;
    // Binary Search Insert and Find
    procedure    BInsert( Item : T; Compare : TListSortCompare ); overload;
    procedure    BInsert( Item : T; Compare : TListSortCompareObj ); overload;
    function     BFind( Item : T; Compare : TListSortCompare; ToTail : Boolean = false ) : Integer; overload;
    function     BFind( Item : T; Compare : TListSortCompareObj; ToTail : Boolean = false  ) : Integer; overload;

    procedure    Clear; virtual;
    procedure    Delete( Index : Integer );
    procedure    DeleteItem( Item : T );
    procedure    Exchange( Index1, Index2: Integer );
    procedure    Extract( Item : T; Collapse : Boolean = False ); overload;
    function     Extract( Item : Integer ) : T; overload;
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

{==============================================================================}
{ TIndexedPersistsList }
{==============================================================================}

type
  EInvalidIndexName   = Exception;
  EInvalidIndex       = Exception;
  EDuplicateIndexName = Exception;
  EItemNotFound       = Exception;

  { TIndexItem }

  TIndexItem = class( TPersists )
  public
    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;
    procedure MakeNew; override;
  end;

  TIndexBase = specialize TPersistsList<TIndexItem>;

  TIndex = class( TIndexBase )
    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;
  end;

  TIndices = specialize TPersistsList<TIndex>;

  generic TIndexedPersistsList<T> = class(TPersists)

  private
    function  GetCapacity: Cardinal;
    procedure SetCapacity(AValue: Cardinal);
  protected
    fList       : array of T;
    fCount      : Cardinal;
    fNextID     : Cardinal;
    fIndices    : TIndices; //array of TIndex;
    fCompareIndex : Integer;
    fIndexIndex : Integer; // -1 means direct un-indexed access
    fPosition   : Integer;

    property     Capacity : Cardinal read GetCapacity write SetCapacity;
    procedure    DeleteItems;
    procedure    Expand;

    function     CompareIndex( Left, Right : Pointer ) : Integer;
  public
    constructor  Create( aParent : TPersists = nil ); override;
    destructor   Destroy; override;
    procedure    UNMODIFY; override;
    procedure    ShowIndex( Idx : Integer );

    procedure    Save( TextIO : TTextIO ); override;
    procedure    Read( TextIO : TTextIO; Version : Integer  ); override;

    function     Insert( Item : T ) : Cardinal;  // sets and returns Item's ID

    function     First : T;
    function     Next  : T;
    function     Prev  : T;
    function     Last  : T;
    function     Bod   : Boolean; // Beginning of data;
    function     Eod   : Boolean; // End of data

    function     Find( Template : T ) : T; // Create the Template, set the values then callfind
                                           // Raises exception if un-indexed access specified

    procedure    SetIndex( theIndexName : String ); // Specify the index to use, null string if un-indexed.

    property     Count : Cardinal read fCount;



  end;

{==============================================================================}
{ TStack }
{==============================================================================}

  generic TStack<T> = class
  private
    SP : Integer;
    Stack : array of T;
  public
    constructor Create;  virtual;
    destructor  Destroy; virtual; // Produces compiler warning if specialization is subclassed

    procedure Push( Value : T ); virtual;
    function  Pop : T; virtual;
  end;

implementation

uses
  ObjectFactory1, Common1, StringSubs, ShowIndexDebug;

{==============================================================================}
{ TIndexItem }
{==============================================================================}

procedure TIndexItem.MakeNew;
begin
  inherited MakeNew;
end;

procedure TIndexItem.Read(TextIO: TTextIO; Version: Integer);
begin
  if Version >= 1 then
    begin
      // Nothing else to do (yet ?)
    end;
end;

procedure TIndexItem.Save(TextIO: TTextIO);
const
  Version = 1;
begin
  SaveHeader( TextIO, Version );
  SaveTrailer( TextIO );
end;

{==============================================================================}
{ TIndex }
{==============================================================================}

procedure TIndex.Read(TextIO: TTextIO; Version: Integer);
var
  I : Integer;
begin
  MakeNew;
  if Version >= 1 then
    begin
      TextIO.ReadLN( fCount );
      SetLength( fList, fCount );
      for I := 0 to pred( fCount ) do
        fList[I] := TIndexItem.Load( TextIO ) as TIndexItem;
    end;
end;

procedure TIndex.Save(TextIO: TTextIO);
const
  TIndexVersion = 1;
var
  I : Integer;
begin
  SaveHeader( TextIO, TIndexVersion );
  TextIO.WriteLn( fCount );
  for I := 0 to pred(Count) do
    fList[I].Save( TextIO );
  SaveTrailer( TextIO );
end;

{==============================================================================}
{ TIndexedPersistsList }
{==============================================================================}

function TIndexedPersistsList.Bod: Boolean;
begin
  Result := fPosition <= 0;
end;

// Parameters ordered like the qsort compare function from C
function TIndexedPersistsList.CompareIndex(Left, Right: Pointer): Integer;
var
  L, R : TIndex;
begin
  L := TIndex(Left);
  R := TIndex(Right);
  Result := 0;
  if L.Name > R.Name then
    Result := 1
  else if L.Name < R.Name then
    Result := -1;
end;

constructor TIndexedPersistsList.Create(aParent: TPersists);
const
  InitialListSize = 16;
var
  I : Integer;
  anIndex : TIndex;
  N       : String;
begin
  inherited Create(aParent);

  fNextID := 0;
  fName := 'Indexed List of ' + T.ClassName;

  SetLength(fList,InitialListSize);
  fIndexIndex := -1; // Un-indexed
  fIndices := TIndices.Create( self );

  // Create the indices
  for I := 0 to pred(T.IndexCount) do
    begin
      anIndex := TIndex.Create( fIndices );
      N := T.IndexName(I);
      anIndex.Name := N;
      fIndices.Add( anIndex );
    end;

end;


procedure TIndexedPersistsList.DeleteItems;
var
  I : Integer;
begin
  for I := 0 to pred(fCount) do
    fList[I].Free;
{ TODO 2 -odonz -cNecessary but not for testing : Clear the indices }
end;

destructor TIndexedPersistsList.Destroy;
begin
  DeleteItems;
  SetLength(fList,0);
  { TODO 2 -odonz -cMemory Leak : Clear the indices }
  inherited Destroy;
end;

function TIndexedPersistsList.Eod: Boolean;
var
  C : Integer;
begin
  C := Count;
  Result := fPosition >= pred(Count);
end;

procedure TIndexedPersistsList.Expand;
begin
  if (Capacity <= fCount) or (Capacity = 0) then
    Capacity := Max(Capacity * 2, 1);
end;

function TIndexedPersistsList.Find(Template: T): T;
var
  S : String;
  Idx : TIndex;
  Itm : TIndexItem;
begin
  Idx := fIndices.Items[fIndexIndex];
  Itm := TIndexItem.Create( Self );
  S := Template.IndexString( fIndexIndex );
  Itm.Name := S;
  fPosition := Idx.BFind( Itm, @CompareIndex );
  Itm.Free;
  if (fPosition >= fCount) or (fPosition < 0) then
    raise EItemNotFound.Create('Can''t find "' + S + '"');
  while fPosition > 0 do
    begin
      if fList[Idx.Items[pred(fPosition)].Order].IndexString( fIndexIndex ) = S then
        Dec(fPosition)
      else
        break;
    end;
  Result := fList[Idx.Items[fPosition].Order];
end;

function TIndexedPersistsList.First: T;
var
  Idx : TIndex;
begin
  fPosition := 0;
  Idx := fIndices.Items[fIndexIndex];
  Result := fList[Idx.Items[fPosition].Order];
end;

function TIndexedPersistsList.GetCapacity: Cardinal;
begin
  Result := Length(fList);
end;

function TIndexedPersistsList.Insert(Item: T): Cardinal;
var
  I, J : Integer;
  P : Integer;
  S : String;
  Index : TIndexItem;
  IDx : Integer;
begin
  Expand;
  Inc(fCount);
  Result := fNextID;
  Item.Id := fNextID;
  fList[fNextID] := Item;
  for I := 0 to pred(fIndices.Count) do
    begin
      fCompareIndex := I;
      Index := TIndexItem.Create( fIndices, Item.IndexString( I ) );
      IDx := Item.ID;
      Index.Order := Item.ID; //fNextID;
      fIndices.Items[I].BInsert( Index, @CompareIndex );
      {$ifdef DEBUG_INSERT}
      ShowIndex( I ); // Debug
      {$endif DEBUG_INSERT}
    end;
  Inc(fNextID);
end;

function TIndexedPersistsList.Last: T;
begin
  fPosition := pred(Count);
  Result := fList[fIndices.Items[fIndexIndex].Items[fPosition].Order];
end;

function TIndexedPersistsList.Next: T;
begin
  Inc(fPosition);
  Result := fList[fIndices.Items[fIndexIndex].Items[fPosition].Order];
end;

function TIndexedPersistsList.Prev: T;
begin
  Dec(fPosition);
  Result := fList[fIndices.Items[fIndexIndex].Items[fPosition].Order];
end;

procedure TIndexedPersistsList.Read(TextIO: TTextIO; Version: Integer);
var
  I : Integer;
begin
  MakeNew;
  if Version >= 1 then
    begin
      TextIO.ReadLn( fCount );
      TextIO.ReadLn( fNextID );
      SetLength(fList, Count );
      for I := 0 to pred( Count ) do
        begin
          fList[I] := T.Load( TextIO ) as T;
          fList[I].Parent := Self as TPersists;
        end;
      fIndices := TIndices.Load( TextIO ) as TIndices;
    end;
end;

procedure TIndexedPersistsList.Save(TextIO: TTextIO);
const
  IndexedPersistsListVersion = 1;
var
  I : Integer;
begin
  SaveHeader( TextIO, IndexedPersistsListVersion );
  TextIO.WriteLn( fCount );
  TextIO.Writeln( fNextID );
  for I := 0 to pred(fCount) do
    fList[I].Save(TextIO);
  fIndices.Save(TextIO);
  SaveTrailer( TextIO );
end;

procedure TIndexedPersistsList.SetCapacity(AValue: Cardinal);
begin
  SetLength(fList,aValue);
end;

procedure TIndexedPersistsList.SetIndex(theIndexName: String);
var
  I : Integer;
  N : String;
begin
  N := Trim(theIndexName);
  if Empty(N) then
    fIndexIndex := -1
  else
    for I := 0 to pred(fIndices.Count) do
      if N = fIndices.Items[I].Name then
        begin
          fIndexIndex := I;
          exit;
        end;
  raise EInvalidIndexName.Create('Attempt to set invalid index name:  ' + N);
end;

procedure TIndexedPersistsList.ShowIndex( Idx : Integer );
var
  I : Integer;
begin
  ShowIndexDebugForm.ListBox1.Clear;
  for I := 0 to pred(Count) do
    ShowIndexDebugForm.ListBox1.Items.Add(fIndices.Items[IDX].Items[I].Name);
  ShowIndexDebugForm.ShowModal;
end;

procedure TIndexedPersistsList.UNMODIFY;
var
  I : Integer;
begin
  inherited UNMODIFY;
  for I := 0 to pred( Count ) do
    fList[I].UNMODIFY;
end;

{==============================================================================}
{ TStack }
{==============================================================================}

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

{==============================================================================}
{ TPersistsList }
{==============================================================================}

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
begin
  inherited;
  fSorted := False;
  fNextID := 0;
  fName := 'List of ' + T.ClassName;

  SetLength(fList,InitialListSize);
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

function TPersistsList.Add(Item: T) : Integer;
begin
  Expand;
  Item.ID := fNextID;
  Inc(fNextID);
  Item.Parent := Self; // DRZ 2013-01-01
  fList[fCount] := Item;
  Inc(fCount);
  Result := Item.ID;
  fSorted := False;
  Modify;
end;

function TPersistsList.Add(Item : T; NoSetID : Boolean ): Integer;
begin
  Expand;
  if not NoSetID then
    begin
      Item.ID := fNextID;
      Inc(fNextID);
    end;
  Item.Parent := Self; // DRZ 2013-01-01
  fList[fCount] := Item;
  Inc(fCount);
  Result := Item.ID;
  fSorted := False;
  Modify;
end;

function TPersistsList.BFind(Item: T; Compare: TListSortCompare; ToTail : Boolean): Integer;
var
  IMax, IMid, IMin : Integer;
  ICompare : Integer;
  theKey, theIndex : TIndexItem;
{$ifdef DEBUG_BFIND}
  L, R : String;
  XX, YY, ZZ : String;
{$endif DEBUG_BFIND}
begin
{$ifdef DEBUG_BFIND}
  YY := ClassName;
  XX := T.ClassName;
  if count > 0 then ZZ := fList[0].ClassName;
{$endif DEBUG_BFIND}
  IMin := 0;
  IMax := pred( Count );
  theKey := TIndexItem.Create( nil );
  theKey.Name := Item.Name;
  try
  while Imax >= IMin do
    begin
      IMid := Imin + ( Imax - Imin ) div 2;
      theIndex := TIndexItem(fList[IMid]);
{$ifdef DEBUG_BFIND}
      L := fList[IMid].name;
      R := theKey.Name;
{$endif DEBUG_BFIND}
      ICompare := Compare( fList[IMid], theKey );
      if ICompare < 0 then
        IMin := IMid + 1
      else if ICompare > 0 then
        IMax := IMid - 1
      else
        begin
          Result := IMid;
          exit;
        end;
    end;
  Result := IMin;
  finally
    theKey.Destroy;
  end;
end;

function TPersistsList.BFind(Item: T; Compare: TListSortCompareObj; ToTail : Boolean): Integer;
var
  IMax, IMid, IMin : Integer;
  ICompare : Integer;
  theKey, theIndex : TIndexItem;
  I : Integer;
{$ifdef DEBUG_BFIND}
  L, R : String;
  XX, YY, ZZ : String;
{$endif DEBUG_BFIND}
begin
{$ifdef DEBUG_BFIND}
  YY := ClassName;
  XX := T.ClassName;
  if count > 0 then ZZ := fList[0].ClassName;
{$endif DEBUG_BFIND}
  IMin := 0;
  IMax := pred( Count );
  theKey := TIndexItem.Create( nil );
  theKey.Name := Item.Name;
  try
  while Imax >= IMin do
    begin
      IMid := Imin + ( Imax - Imin ) div 2;
      theIndex := TIndexItem(fList[IMid]);
{$ifdef DEBUG_BFIND}
      L := fList[IMid].name;
      R := theKey.Name;
{$endif DEBUG_BFIND}
      ICompare := Compare( fList[IMid], theKey );
      if ICompare < 0 then
        IMin := IMid + 1
      else if ICompare > 0 then
        IMax := IMid - 1
      else
        begin
          // If we have exact match, step forward or backward as specified in
          // ToTail
          if ToTail then
            begin

              { TODO 1 -oDon Z -cIndex Searching or Building : Implement fwd or reverse step till first or last duplicate entry found. }
            end
          else
            begin
              while pred(IMin) > 0 do  //
            end;
          Result := IMid;
          exit;
        end;
    end;
  Result := IMin;
  finally
    theKey.Destroy;
  end;
end;

procedure TPersistsList.BInsert(Item: T; Compare: TListSortCompareObj);
var
  Pos : Integer;
begin
  Pos := BFind( Item, Compare );
  Insert( Pos, Item );
  fSorted := True;;
end;

procedure TPersistsList.BInsert(Item: T; Compare: TListSortCompare);
var
  Pos : Integer;
begin
  Pos := BFind( Item, Compare );
  Insert( Pos, Item );
  fSorted := True;
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
    begin
//      fList[I].Free;
//      fList[I] := nil;
    end;
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
  if (fCount >= Capacity) or (Capacity = 0) then
    Capacity := Capacity * 2 +1;
end;

function TPersistsList.Extract(Item: Integer): T;
var
  I : Integer;
begin
  Result := Items[Item];
  for I := succ(Item) to pred(fCount) do
    Items[I-1] := Items[I];
end;

procedure TPersistsList.Extract(Item: T; Collapse : Boolean );
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

function TPersistsList.First: T;
begin
  Result := Items[0];
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
  Expand;
  CheckCount( Index );
  for I := fCount-2 downto Index do
    Items[I+1] := Items[I];
  Item.ID := fNextID;
  Inc(fNextID);
  Items[Index] := Item;
  fSorted := False;
  Modify;
end;

function TPersistsList.Last: T;
begin
  Result := Items[pred(fCount)];
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
    T(fList[I]).Save( TextIO );
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

initialization
//ObjectFactory.RegisterClass( TIndexedPersistsList.ClassType );
ObjectFactory.RegisterClass( TindexItem.ClassType );
ObjectFactory.RegisterClass( TIndices.ClassType );
ObjectFactory.RegisterClass( TIndex.ClassType );

end.

