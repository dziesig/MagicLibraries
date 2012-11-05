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

      function    Readln( var Line : String ) : Integer;
      function    Readln( var Int  : Integer ) : Integer;
      function    Readln( var Dbl  : Double ) : Integer;
      function    Readln( var Bool : Boolean ) : Integer;

      procedure   Writeln( Line : String );
      procedure   Writeln( Int  : Integer );
      procedure   Writeln( Dbl  : Double );
      procedure   Writeln( Bool : Boolean );

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

function TTextIO.Readln(var Int: Integer): Integer;
begin
  Inc(fLineNo);
  Result := fLineNo;
  try
    System.ReadLn( fFile, Int );
  except
    MessageBox('Invalid numeric format at line ' + IntToStr( fLineNo ) );
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

end.

