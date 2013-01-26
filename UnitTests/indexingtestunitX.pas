unit IndexingTestUnitX;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1, Generics1, TextIO1;

type

  { TNameOnly }

  TNameOnly = class( TPersists )
  public
    constructor Create( aParent : TPersists ) ; override;

    function  IndexString( theIndex : Cardinal ) : String; override;
    class function  IndexCount : Cardinal; override;
    class function  IndexName( Index : Cardinal ) : String ; override;

    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;
    procedure MakeNew; override;
  end;

//  TNameOnlyListBase = specialize TIndexedPersistsList< TNameOnly >;
//  TNameOnlyList = specialize TIndexedPersistsList< TNameOnly >;
TNameOnlyList = class(TPersists)
type
  TIndex = specialize TPersistsList<TPersists>;
  TIndices = specialize TPersistsList<TIndex>;

private
  function  GetCapacity: Cardinal;
  procedure SetCapacity(AValue: Cardinal);
protected
  fList       : array of TNameOnly;
  fCount      : Cardinal;
  fNextID     : Cardinal;
  fIndices    : TIndices; //array of TIndex;
  fIndexIndex : Integer; // -1 means direct un-indexed access
  fPosition   : Integer;

  property     Capacity : Cardinal read GetCapacity write SetCapacity;
  procedure    DeleteItems;
  procedure    Expand;
public
  constructor  Create( aParent : TPersists = nil ); override;
  destructor   Destroy; override;
  procedure    UNMODIFY; override;

  procedure    Save( TextIO : TTextIO ); override;
  procedure    Read( TextIO : TTextIO; Version : Integer  ); override;

  function     Insert( Item : TNameOnly ) : Cardinal;  // sets and returns Item's ID

  function     First : TNameOnly;
  function     Next  : TNameOnly;
  function     Prev  : TNameOnly;
  function     Last  : TNameOnly;
  function     Bod   : Boolean; // Beginning of data;
  function     Eod   : Boolean; // End of data

  function     Find( Template : TNameOnly ) : TNameOnly; // Create the Template, set the values then callfind
                                         // Raises exception if un-indexed access specified

  procedure    SetIndex( theIndexName : String ); // Specify the index to use, null string if un-indexed.

  property     Count : Cardinal read fCount;



end;

  //{ TNameOnlyList }
  //
  //TNameOnlyList = class(TNameOnlyListBase)
  //  constructor Create( aParent : TPersists ); override;
  //end;

  procedure NameOnlyIndexTest( TestNumber : Integer );


implementation

uses
  ObjectFactory1, Common1, Stringsubs;

procedure NameOnlyIndexTest( TestNumber : Integer );
var
  TestList : TNameOnlyList;
  Itm      : TNameOnly;
  TextIO   : TTextIO;
begin
  case TestNumber of
    0:
      begin
        TestList := TNameOnlyList.Create(nil);
        Itm := TNameOnly.Create(TestList);
        Itm.Name := 'C';
        TestList.Insert( Itm );
        Itm := TNameOnly.Create(TestList);
        Itm.Name := 'B';
        TestList.Insert( Itm );
        Itm := TNameOnly.Create(TestList);
        Itm.Name := 'A';
        TestList.Insert( Itm );
        TextIO := TTextIO.Create( DefaultSaveLocation + 'Test1.tst',True);
        try
          TestList.Save( TextIO );
        finally
          TextIO.Free;
        end;
        TestList.Free;
      end;
    1:;
    2:;
    3:;
  end;
end;

{ TNameOnly }

const
  theIndexCount = 2;

  IndexNames : array[0..pred(theIndexCount)] of String =
    ('Name','Id');

function TNameOnlyList.Bod: Boolean;
begin
  Result := fPosition <= 0;
end;

constructor TNameOnlyList.Create(aParent: TPersists);
const
  InitialListSize = 16;
var
  I : Integer;
  anIndex : TIndex;
  N       : String;
  C       : Integer;
begin
  inherited Create(aParent);

  fNextID := 0;
  fName := 'Indexed List of ' + TNameOnly.ClassName;

  SetLength(fList,InitialListSize);
  fIndexIndex := -1; // Un-indexed
  fIndices := TIndices.Create( self );

  // Create the indices
  C := TNameOnly.IndexCount;
  for I := 0 to pred(C) do
    begin
      anIndex := TIndex.Create( fIndices );
      N := TNameOnly.IndexName(I);
      anIndex.Name := N;
      fIndices.Add( anIndex );
    end;

end;

procedure TNameOnlyList.DeleteItems;
var
  I : Integer;
begin
  for I := 0 to pred(fCount) do
    fList[I].Free;
{ TODO 2 -odonz -cNecessary but not for testing : Clear the indices }
end;

destructor TNameOnlyList.Destroy;
begin
DeleteItems;
SetLength(fList,0);
{ TODO 2 -odonz -cMemory Leak : Clear the indices }
inherited Destroy;
end;

function TNameOnlyList.Eod: Boolean;
begin
  Result := fPosition >= Count;
end;

procedure TNameOnlyList.Expand;
begin
  if (Capacity <= fCount) or (Capacity = 0) then
    Capacity := Max(Capacity * 2, 1);
end;

function TNameOnlyList.Find(Template: TNameOnly): TNameOnly;
begin

end;

function TNameOnlyList.First: TNameOnly;
var
  Idx : TIndex;
begin
  fPosition := 0;
  Idx := fIndices.Items[fIndexIndex];
  Result := fList[Idx.Items[fPosition].Order];
end;

function TNameOnlyList.GetCapacity: Cardinal;
begin
  Result := Length(fList);
end;

function TNameOnlyList.Insert(Item: TNameOnly): Cardinal;
var
  I, J : Integer;
  P : Integer;
  S : String;
  C : Integer;
  N : Integer;
begin
  Expand;
  Inc(fCount);
  Result := fNextID;
  fList[fNextID] := Item;
  Inc(fNextID);
  C := fIndices.Count;
  for I := 0 to pred(C) do
    begin
      N := fCount;
      for J := 0 to pred(N) do
        begin
          Stub('Index string : ' + IntToStr(I) + ' ' + IntToStr( J ));
          S := fList[J].IndexString( I );
          Stub('Index string : ' + S);
        end;
    end;
end;

function TNameOnlyList.Last: TNameOnly;
begin
  fPosition := pred(Count);
  Result := fList[fIndices.Items[fIndexIndex].Items[fPosition].Order];
end;

function TNameOnlyList.Next: TNameOnly;
begin
  Inc(fPosition);
  Result := fList[fIndices.Items[fIndexIndex].Items[fPosition].Order];
end;

function TNameOnlyList.Prev: TNameOnly;
begin
  Dec(fPosition);
  Result := fList[fIndices.Items[fIndexIndex].Items[fPosition].Order];
end;

procedure TNameOnlyList.Read(TextIO: TTextIO; Version: Integer);
begin
  Stub('TIndexedPersistsList.Read');
end;

procedure TNameOnlyList.Save(TextIO: TTextIO);
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

procedure TNameOnlyList.SetCapacity(AValue: Cardinal);
begin
  SetLength(fList,aValue);
end;

procedure TNameOnlyList.SetIndex(theIndexName: String);
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

procedure TNameOnlyList.UNMODIFY;
var
  I : Integer;
begin
  inherited UNMODIFY;
  for I := 0 to pred( Count ) do
    fList[I].UNMODIFY;
end;

{ TNameOnlyList }

//constructor TNameOnlyList.Create(aParent: TPersists);
//begin
//  inherited Create(aParent);
//end;

constructor TNameOnly.Create(aParent: TPersists);
begin
  inherited Create(aParent);
end;

class function TNameOnly.IndexCount: Cardinal;
begin
  Result := theIndexCount;
end;

class function TNameOnly.IndexName(Index: Cardinal): String;
begin
  Result := IndexNames[Index];
end;

function TNameOnly.IndexString(theIndex : Cardinal): String;
begin
  case theIndex of
    0: Result := Name;
    1: Result := IntToStr(ID);
  else
    raise EInvalidIndex.Create('Invalid index :  ' + IntToStr( theIndex ) );
  end;
end;

procedure TNameOnly.MakeNew;
begin
  inherited MakeNew;
end;

procedure TNameOnly.Read(TextIO: TTextIO; Version: Integer);
begin
  Stub('TNameOnly.Read');
end;

procedure TNameOnly.Save(TextIO: TTextIO);
const
  Version = 1;
begin
  SaveHeader( TextIO, Version );
  TextIO.WriteLn( 'A Test Record ' + DateTimeToStr( Now ));
  SaveTrailer( TextIO );
end;

initialization

  ObjectFactory.RegisterClass( TNameOnly.ClassType );
  ObjectFactory.RegisterClass( TNameOnlyList.ClassType );

end.

