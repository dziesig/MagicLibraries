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

unit persists1;

{$mode objfpc}{$H+}

{$M+} // Enables RTTI

interface

uses
  Classes, SysUtils, StdCtrls,

  TextIO1;

type

  { TPersists }

  TPersists = class
  private
    fOnChange: TNotifyEvent;
    fParent   : TPersists;
    procedure SetID(const AValue: Integer);
    procedure SetModified(const AValue: Boolean);
    procedure SetOrder(AValue: Cardinal);
  protected
    fModified : Boolean;
    fName     : String;
    fID       : Integer;
    fOrder    : Cardinal;
    procedure Modify;
    function  IsModified : Boolean; virtual;
    procedure SetName( Value : String );
  public
    constructor Create( aParent : TPersists = nil); virtual;
    constructor Create( aParent : TPersists; AName : String ); virtual; overload;
    procedure MakeNew; virtual;
    procedure Save( TextIO : TTextIO ); virtual; abstract;
    procedure SaveHeader( TextIO : TTextIO; Version : Integer );
    procedure SaveTrailer( TextIO : TTextIO );
    class function Load( TextIO : TTextIO ) :TPersists;
    procedure Read( TextIO : TTextIO; Version : Integer ); virtual; abstract;

    procedure Show( Memo : TMemo ); virtual; abstract;

    procedure Assign( Source : TPersists ); virtual;
    procedure AssignTo( Dest : TPersists ); virtual;

    procedure Update( var Data : Integer; NewValue : Integer ); overload;
    procedure Update( var Data : Cardinal; NewValue : Integer ); overload;
    procedure Update( var Data : Double;  NewValue : Double );  overload;
    procedure Update( var Data : String;  NewValue : String );  overload;
    procedure Update( var Data : Boolean; NewValue : Boolean );  overload;

    procedure UNMODIFY; virtual; { LOOK HERE there are only a few places where this is valid }

    property Modified : Boolean read IsModified write SetModified;
    property Parent : TPersists read fParent write fParent;
    property Name   : String read fName write SetName;
    property ID     : Integer read fID write SetID;
    property Order  : Cardinal read fOrder write SetOrder;
    property OnChange : TNotifyEvent read fOnChange write fOnChange;
  end;

implementation

uses
  ObjectFactory1;

{ TPersists }

procedure TPersists.Assign(Source: TPersists);
begin
  fModified := false;
  fParent := Source.Parent;
  fName   := Source.Name + '(copy)';
end;

procedure TPersists.AssignTo(Dest: TPersists);
begin
  Dest.fModified := False;
  Dest.Parent := Parent;
  Dest.fName := Name + '<copy>';
end;

constructor TPersists.Create(aParent: TPersists);
begin
  fParent := aParent;
  fId := 0; // TPersistsList will set this where necessary;
  MakeNew;
end;

constructor TPersists.Create(aParent: TPersists; AName: String);
begin
  fName := AName;
  fParent := aParent;
  fId := 0; // TPersistsList will set this where necessary;
  MakeNew;
end;

function TPersists.IsModified: Boolean;
begin
  Result := fModified;
end;

class function TPersists.Load(TextIO: TTextIO) : TPersists;
var
  ClsName : String;
  S       : String;
  Version : Integer;
  TempID  : Integer;
  TempOrder : Cardinal;
  NN      : String;
  procedure CheckEndClass(FileClass, ExpectedClass: String; TextIO : TTextIO);
  var
    Cls : String;
    Len : Integer;
    Line : Integer;
    H, T : String;
  begin
    Len := Length( FileClass );
    Cls := Copy( FileClass,3,Len-3);
    Line := TextIO.LineNo;
    H := Copy( FileClass,1,2);
    T := Copy( FileClass,Len,1);
    if (H <> '</') or (T <> '>') then
      raise Exception.Create( 'Invalid End of Class format [' +
                              FileClass + '], expecting ' + ExpectedClass +
                              ' at line ' + IntToStr( TextIO.LineNO ));
    if Cls <> ExpectedClass then
      raise Exception.Create( 'End of Class mismatch.  ' + Cls + ' found, '+
                              ExpectedClass + ' expected at line ' +
                              IntToStr( TextIO.LineNo) );
  end;
  procedure CheckStartClass(var FileClass : String; TextIO : TTextIO);
  var
    Cls : String;
    Len : Integer;
  begin
    Len := Length( FileClass );
    Cls := Copy( FileClass,2,Len-2);
    if (Copy( FileClass,1,1) <> '<') or (Copy( FileClass,Len,1) <> '>') then
      raise Exception.Create( 'Invalid Start of Class format [' +
                              FileClass + '] at line ' + IntToStr(TextIO.LineNo) );
    FileClass := Cls;
  end;

begin
  Version := 0;
  S := '';
  ClsName := self.ClassName;    // Get the expected class name
  TextIO.ReadLn(S);             // Read the start of class
  CheckStartClass(S, TextIO);           // Assert they are correct and of correct format
  ClsName := S;
  TextIO.Readln(Version);       // Read the Object's version
  TextIO.Readln( NN );           // Read the object's name
  TextIO.ReadLn( TempID );      // Read the Object's ID;
  TextIO.Readln( TempOrder );   // Read the Object's Order
  Result := ObjectFactory.MakeObject( ClsName ) as TPersists;
  Result.Read( TextIO, Version );
  Result.fName := NN;
  Result.fID := TempID;
  Result.fOrder := TempOrder;
  TextIO.Readln(S);             // Read the end of class
  CheckEndClass(S,ClsName, TextIO);     // Assert end of class is correct and of correct format
  Result.UNMODIFY;              // make sure this was NOT modified by the load.
end;

procedure TPersists.MakeNew;
begin
  fModified := false;
end;

procedure TPersists.Modify;
begin
  fModified := true;
  if fParent <> nil then
    fParent.Modify;
  if Assigned( fOnChange ) then fOnChange( self as TObject );
end;

procedure TPersists.SaveHeader(TextIO: TTextIO; Version : Integer);
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('<'+S+'>');    // Write the start of class
  TextIO.Writeln( Version );
  TextIO.Writeln(Name);         // Write the Object's Name
  TextIO.Writeln(fID);          // Write the object's Id field
  TextIO.Writeln(fOrder);
end;

procedure TPersists.SaveTrailer(TextIO: TTextIO);
var
  S : String;
begin
  S := self.ClassName;          // Get our class name
  TextIO.Writeln('</'+S+'>');   // Write the end of class
  UNMODIFY;                     // if it were modified, it isn't any more.
end;

procedure TPersists.SetModified(const AValue: Boolean);
begin
  fModified:=AValue;
  if fModified then
    if fParent <> nil then
      fParent.Modify;
end;

procedure TPersists.SetOrder(AValue: Cardinal);
begin
  Update( fOrder, AValue);
end;

procedure TPersists.SetID(const AValue: Integer);
begin
  if fID=AValue then exit;
  if fID > 0 then
    raise Exception.Create('Attempt to change object ID');
  fID:=AValue;
end;

procedure TPersists.SetName(Value: String);
begin
  Update(fName,Value);
end;

procedure TPersists.UNMODIFY;
begin
  fModified := false;
  if Assigned( fParent) then
    if fParent.Modified then
      fParent.UNMODIFY;
end;

{ 2012-12-21 - Not the end of the world }
{ Changed the order of Modify and Data update so the modify event handler }
{ has the updated value.                                                  }
procedure TPersists.Update(var Data: Boolean; NewValue: Boolean);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TPersists.Update(var Data: Double; NewValue: Double);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TPersists.Update(var Data: String; NewValue: String);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TPersists.Update(var Data: Integer; NewValue: Integer);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

procedure TPersists.Update(var Data: Cardinal; NewValue: Integer);
begin
  if Data <> NewValue then
    begin
      Data := NewValue;
      Modify;
    end;
end;

end.
