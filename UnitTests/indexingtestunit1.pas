unit IndexingTestUnit1;

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

  TNameOnlyList = specialize TIndexedPersistsList< TNameOnly >;

  procedure NameOnlyIndexTest( TestNumber : Integer );


implementation

uses
  ObjectFactory1, Common1;

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
        Itm.Name := 'E';
        TestList.Insert( Itm );
        Itm := TNameOnly.Create(TestList);
        Itm.Name := 'D';
        TestList.Insert( Itm );
        Itm := TNameOnly.Create(TestList);
        Itm.Name := 'F';
        TestList.Insert( Itm );
        Itm := TNameOnly.Create(TestList);
        Itm.Name := 'A';
        TestList.Insert( Itm );

        TestList.ShowIndex( 0 );
        TestList.ShowIndex( 1 );
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
  ObjectFactory.RegisterClass( TNameOnlyList );

end.

