unit IndexingTestUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Persists1, Generics1;

type
  TNameOnly = class( TPersists )

  end;

  TNameOnlyList = specialize TPersistsList< TNameOnly >;

  procedure NameOnlyIndexTest( TestNumber : Integer );


implementation

uses
  ObjectFactory1;

procedure NameOnlyIndexTest( TestNumber : Integer );
begin

end;

initialization

  ObjectFactory.RegisterClass( TNameOnly.ClassType );
  ObjectFactory.RegisterClass( TNameOnlyList );

end.

