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

unit TextIO1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls;

type

  { TTextIO }

  { Note:  TTextIO uses a (possibly hidden) TMemo to handle the individual }
  {        lines in the file.  For Debugging purposes, the TMemo may be    }
  {        made visible, but normally it will be hidden                    }

  TTextIO = class( TObject )
    private
      fMode       : Integer;
      fLineNo     : Integer;
      fFile       : Text;
      fPath       : String;

    public
      constructor Create( aPath : String; Output : Boolean );
      destructor  Destroy; override;

      function    Readln( var Line : String ) : Integer; overload;
      function    Readln( var Int  : Integer ) : Integer;  overload;
      function    Readln( var Dbl  : Double ) : Integer;  overload;
      function    Readln( var Bool : Boolean ) : Integer; overload;
      function    ReadLn( var Card : Cardinal ) : Integer;   overload;

      procedure   Writeln( Line : String );    overload;
      procedure   Writeln( Int  : Integer );   overload;
      procedure   Writeln( Dbl  : Double );     overload;
      procedure   Writeln( Bool : Boolean );    overload;
      procedure   WriteLn( Card : Cardinal );   overload;

      property    LineNo : Integer read fLineNo;
      property    Path : String read fPath;

  end;

implementation

{ TTextIO }

uses
  Common1;

constructor TTextIO.Create( aPath : String; Output : Boolean);
begin
  inherited Create;
  fLineNo := 0;
  fPath := aPath;
  AssignFile( fFile, aPath );
  if Output then
    Rewrite( fFile )
  else
    Reset( fFile );
end;

destructor TTextIO.Destroy;
begin
  CloseFile( fFile );
end;



function TTextIO.Readln(var Line: String): Integer;
begin
  Inc(fLineNo);
  Result := fLineNo;
  System.ReadLn( fFile, Line );
end;

procedure TTextIO.Writeln(Bool: Boolean);
var
  Txt : String;
begin
  Inc(fLineNo);
  if Bool then
    Txt := 'TRUE'
  else
    Txt := 'FALSE';
  System.Writeln( fFile, Txt );
end;

procedure TTextIO.WriteLn(Card: Cardinal);
begin
  Inc( fLineNo );
  System.Writeln( fFile, Card );
end;

function TTextIO.Readln(var Int: Integer): Integer;
begin
  Inc(fLineNo);
  Result := fLineNo;
  try
    System.ReadLn( fFile, Int );
  except
    MessageBox('Invalid numeric (Integer) format at line ' + IntToStr( fLineNo ) );
    raise;
  end;
end;

function TTextIO.Readln(var Dbl: Double): Integer;
begin
  Inc(fLineNo);
  Result := fLineNo;
  System.ReadLn( fFile, Dbl );
end;

procedure TTextIO.Writeln(Line: String);
begin
  Inc( fLineNo );
  System.Writeln( fFile, Line );
end;

procedure TTextIO.Writeln(Int: Integer);
begin
  Inc( fLineNo );
  System.Writeln( fFile, Int );
end;

procedure TTextIO.Writeln(Dbl: Double);
begin
  Inc( fLineNo );
  System.Writeln( fFile, Dbl );
end;

function TTextIO.Readln(var Bool: Boolean): Integer;
var
  Txt : String;
begin
  Inc(fLineNo);
  Result := fLineNo;
  System.ReadLn( fFile, Txt );
  Bool := Txt = 'TRUE';
end;

function TTextIO.ReadLn(var Card: Cardinal): Integer;
begin
  Inc(fLineNo);
  Result := fLineNo;
  try
    System.ReadLn( fFile, Card );
  except
    MessageBox('Invalid numeric (Cardinal) format at line ' + IntToStr( fLineNo ) );
    raise;
  end;
end;

end.
