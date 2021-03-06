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

unit stringsubtestform2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, baseform2,
  Stringsubs;

type

  { TForm11 }

  TForm11 = class(TForm1)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure DoTest( Passed : Boolean; Test : String; NegatePassed : Boolean = False ); overload;
    procedure DoTest( Funct : TStringFunctionTest; Argument : Extended; theWidth : Integer; ExpectedResult : String; Test : String; ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TBooleanFunctionTest; Argument : String; ExpectedResult : Boolean; Test : String; ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TExtendedFunctionTest; Argument : String; ExpectedResult : Extended; Test : String; ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TStringIntFunctionTest; Argument : String; ExpectedResult : Integer; Test : String; ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TIndentByString; Argument: String; Count, BaseWidth : Integer; ExpectedResult : String; Test : String; ExceptionExpected : Boolean = False); overload;
    procedure DoTest( Funct : TIndentByFile; Count, BaseWidth : Integer; ExpectedResult : String; Test : String; ExceptionExpected : Boolean = False); overload;

    procedure DollarsToFloatTest;
    procedure FloatToDollarsTest;
    procedure FloatToPercentTest;
    procedure EmptyTest;
    procedure PercentToFloatTest;
    procedure StringToFloatTest;
    procedure StringToIntTest;
    procedure IndentByStringTest;
    procedure IndentByFileTest;

    function EmptyTestFunc( Value : String ) : Boolean;
    function FloatToDollarsFunc( Value : Extended; theWidth : Integer) : String;
    function FloatToPercentFunc( Value : Extended; theWidth : Integer) : String;
    function DollarsToFloatFunc( Value : String ) : Extended;
    function PercentToFloatFunc( Value : String ) : Extended;
    function StringToFloatFunc( Value : String ) : Extended;
    function StringToIntFunc( Value : String ) : Integer;
    function IndentByStringFunc( Value : String; Count, BaseIndent : Integer ) : String;
    function IndentByFileFunc( Count, BaseIndent : Integer ) : String;
  public
    { public declarations }
    procedure RunSelectedTests; override;
  end;

var
  Form11: TForm11;

implementation

{$R *.lfm}

{ TForm11 }

{ The following must correspond to the order of check items in CheckGroup1     }

const
  DollarsToFloatCI = 0;
  EmptyCI          = 1;
  FloatToDollarsCI = 2;
  FloatToPercentCI = 3;
  PercentToFloatCI = 4;
  StringToFloatCI  = 5;
  StringToIntCI    = 6;
  IndentByStringCI = 7;
  IndentByFileCi   = 8;

procedure TForm11.FormCreate(Sender: TObject);
begin
  Caption := 'StringSub Unit Tests';
  CheckGroup1.Items.Add( 'Dollars To Float' );
  CheckGroup1.Items.Add( 'Empty' );
  CheckGroup1.Items.Add( 'Float To Dollars' );
  CheckGroup1.Items.Add( 'Float To Percent' );
  CheckGroup1.Items.Add( 'Percent To Float' );
  CheckGroup1.Items.Add( 'String To Float' );
  CheckGroup1.Items.Add( 'String To Int' );
  CheckGroup1.Items.Add( 'IndentBy String' );
  CheckGroup1.Items.Add( 'IndentBy File' );
end;

procedure TForm11.DoTest(Passed: Boolean; Test: String; NegatePassed: Boolean);
var
  Pass : Boolean;
begin
  Pass := Passed xor NegatePassed;
  if Pass then
    Log(IndentBy( Test + ' Passed',2))
  else
    Log(IndentBy( Test + ' Failed',2), True)
end;

procedure TForm11.DoTest(Funct: TStringFunctionTest; Argument: Extended;
  theWidth: Integer; ExpectedResult: String; Test: String;
  ExceptionExpected: Boolean);
var
  Res : String;
  Pass : Boolean;
begin
  Res := Funct( Argument, theWidth );
  Pass := (Res = ExpectedResult);
  if Pass then
    Log(IndentBy( Test + ' Passed',2))
  else
    Log(IndentBy( Test + ' Failed.  Expected:  "' + ExpectedResult + '", got:  "' + Res + '"',2), True)
end;

procedure TForm11.DoTest(Funct: TBooleanFunctionTest; Argument: String;
  ExpectedResult: Boolean; Test: String; ExceptionExpected: Boolean);
var
  Result : Boolean;
  Pass   : Boolean;
begin
  Result := Funct( Argument );
  Pass   := (Result = ExpectedResult);
  if Pass then
    Log(IndentBy( Test + ' Passed',2))
  else
    Log(IndentBy( Test + ' Failed.  Expected:  "' + BoolToStr(ExpectedResult) + '", got:  "' + BoolToStr(Result) + '"',2), True)
end;

procedure TForm11.DoTest(Funct: TExtendedFunctionTest; Argument: String;
  ExpectedResult: Extended; Test: String; ExceptionExpected: Boolean);
var
  Result : Extended;
begin
  try
    Result := Funct( Argument);
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log(IndentBy( Test + ' Failed.  Expected:  "' + FloatToStr(ExpectedResult) + '", got:  "' + FloatToStr(Result) + '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;
end;

procedure TForm11.DoTest(Funct: TStringIntFunctionTest; Argument: String;
  ExpectedResult: Integer; Test: String; ExceptionExpected: Boolean);
var
  Result : Integer;
begin
  try
    Result := Funct( Argument);
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2),true)
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log(IndentBy( Test + ' Failed.  Expected:  "' + IntToStr(ExpectedResult) + '", got:  "' + IntToStr(Result) + '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2), true)
  end;
end;

procedure TForm11.DoTest(Funct: TIndentByString; Argument: String; Count,
  BaseWidth: Integer; ExpectedResult: String; Test: String;
  ExceptionExpected: Boolean);
var
  Result : String;
begin
  try
    Result := Funct( Argument, Count, BaseWidth );
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2), true)
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log(IndentBy( Test + ' Failed.  Expected:  "' + ExpectedResult + '", got:  "' + Result + '"',2), True);
  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2), true)
  end;
end;

procedure TForm11.DoTest(Funct: TIndentByFile; Count, BaseWidth: Integer;
  ExpectedResult: String; Test: String; ExceptionExpected: Boolean);
var
  Result : String;
begin
  try
    Result := Funct( Count, BaseWidth );
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2),true)
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log(IndentBy( Test + ' Failed.  Expected:  "' + ExpectedResult + '", got:  "' + Result + '"',2), True);
  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2),true)
  end;
end;

procedure TForm11.DollarsToFloatTest;
begin
  Log('Start of "function DollarsToFloat(Value: String): Double;"' );
  DoTest(@DollarsToFloatFunc, '', 0.0, 'DollarsToFloat('''')',false);
  DoTest(@DollarsToFloatFunc, '$', 0.0, 'DollarsToFloat(''$'')',false);
  DoTest(@DollarsToFloatFunc, '$0.00', 0.0, 'DollarsToFloat(''$0.00'')',false);
  DoTest(@DollarsToFloatFunc, '$1,000.00', 1000.0, 'DollarsToFloat(''$1,000.00'')',false);
  DoTest(@DollarsToFloatFunc, '1.23', 1.23, 'DollarsToFloat(''1.23'')',false);
  DoTest(@DollarsToFloatFunc, '1.23$', 1.23, 'DollarsToFloat(''1.23$'')',false);
  DoTest(@DollarsToFloatFunc, '1234', 1234.0, 'DollarsToFloat(''1234'')',false);
  DoTest(@DollarsToFloatFunc, '1.2.3', 1.23, 'DollarsToFloat(''1.2.3'')',true);
  DoTest(@DollarsToFloatFunc, '3a', 1.23, 'DollarsToFloat(''3a'')',true);
  Log('End of   "function DollarsToFloat(Value: String): Double;"' );
  Log('');
end;

procedure TForm11.FloatToDollarsTest;
begin
  Log('Start of "function FloatToDollars( Value : Extended; Width : Integer = 10 ) : String;"' );
  DoTest( @FloatToDollarsFunc, 0, 0, '$     0.00', 'FloatToDollars( 0 )',False);
  DoTest( @FloatToDollarsFunc, 0, 6, '$ 0.00', 'FloatToDollars( 0, 6 )',False);
  DoTest( @FloatToDollarsFunc, 10, 6, '$10.00', 'FloatToDollars( 10, 6 )',False);
  DoTest( @FloatToDollarsFunc, -10, 6, '$-10.00', 'FloatToDollars( -10, 6 )',False);
  DoTest( @FloatToDollarsFunc, 1000000.555, 0, '$1,000,000.56', 'FloatToDollars( 1000000.555 )',False);
  DoTest( @FloatToDollarsFunc, 1000000.555, 15, '$  1,000,000.56', 'FloatToDollars( 1000000.555, 15 )',False);
  Log('End of   "function FloatToDollars( Value : Extended; Width : Integer = 10 ) : String;"' );
  Log('');
end;

procedure TForm11.FloatToPercentTest;
begin
  Log('Start of "function FloatToPercent( Value : Extended; Width : Integer = 8 ) : String;"');
  DoTest( @FloatToPercentFunc, 0.0, 0, '   0.00%', 'FloatToPercent(0.0)', false );
  DoTest( @FloatToPercentFunc, 0.0, 1, '0.00%', 'FloatToPercent(0.0,1)', false );
  DoTest( @FloatToPercentFunc, 1.0, 0, ' 100.00%', 'FloatToPercent(1.0)', false );
  DoTest( @FloatToPercentFunc, 100.0, 0, '10,000.00%', 'FloatToPercent(100.0)', false );
  DoTest( @FloatToPercentFunc, -0.5, 10, '   -50.00%', 'FloatToPercent(-0.5)',false );
  Log('End of   "function FloatToPercent( Value : Extended; Width : Integer = 8 ) : String;"');
  Log('');
end;

procedure TForm11.EmptyTest;
begin
  Log('Start of "function Empty( S : String ) : Boolean;"' );
  DoTest( @EmptyTestFunc, '', True, 'Empty( '''' )', False);
  DoTest( @EmptyTestFunc, '   ', True, 'Empty( ''   '' )', False);
  DoTest( @EmptyTestFunc, '   ', True, 'Empty( ''   '' )', False);
  DoTest( @EmptyTestFunc,'   ' + #8, True, 'Empty( ''    ''<Tab>)', false);
  DoTest( @EmptyTestFunc,#8#8#8, True, 'Empty( <Tab><Tab><Tab>)', false);
  DoTest( @EmptyTestFunc,#0, True, 'Empty( <nul>)', false);

  DoTest( @EmptyTestFunc, 'ABC', False, 'Empty(''ABC'')', false);
  Log('End of   "function Empty( S : String ) : Boolean;"');
  Log('');
end;

procedure TForm11.PercentToFloatTest;
begin
  Log('Start of "function PercentToFloat( Value : String ) : Extended;"' );
  DoTest(@PercentToFloatFunc, '', 0.0, 'PercentToFloat('''')',false);
  DoTest(@PercentToFloatFunc, '%', 0.0, 'PercentToFloat(''%'')',false);
  DoTest(@PercentToFloatFunc, '0.00%', 0.0, 'PercentToFloat(''0.00%'')',false);
  DoTest(@PercentToFloatFunc, '1,000.00%', 10.0, 'PercentToFloat(''1,000.00%'')',false);
  DoTest(@PercentToFloatFunc, '1.23', 0.0123, 'PercentToFloat(''1.23'')',false);
  DoTest(@PercentToFloatFunc, '1.23%', 0.0123, 'PercentToFloat(''1.23%'')',false);
  DoTest(@PercentToFloatFunc, '1234', 12.340, 'PercentToFloat(''1234'')',false);
  DoTest(@PercentToFloatFunc, '1.2.3', 1.23, 'PercentToFloat(''1.2.3'')',true);
  DoTest(@PercentToFloatFunc, '3a', 1.23, 'PercentToFloat(''3a'')',true);

  Log('End of   "function PercentToFloat( Value : String ) : Extended;"' );
  Log('');

end;

procedure TForm11.StringToFloatTest;
begin
  Log('Start of "function StringToFloat( Value : String ) : Extended;"' );
  DoTest(@StringToFloatFunc, '', 0.0, 'StringToFloat('''')',false);
  DoTest(@StringToFloatFunc, '0.00', 0.0, 'StringToFloat(''0.00'')',false);
  DoTest(@StringToFloatFunc, '1,000.00', 1000.0, 'StringToFloat(''1,000.00'')',false);
  DoTest(@StringToFloatFunc, '1.23', 1.23, 'StringToFloat(''1.23'')',false);
  DoTest(@StringToFloatFunc, '1234', 1234.0, 'StringToFloat(''1234'')',false);
  DoTest(@StringToFloatFunc, '1.2.3', 1.23, 'StringToFloat(''1.2.3'')',true);
  DoTest(@StringToFloatFunc, '3a', 1.23, 'StringToFloat(''3a'')',true);
  Log('End of   "function StringToFloat( Value : String ) : Extended;"' );
  Log('');
end;

procedure TForm11.StringToIntTest;
begin
  Log('Start of "function StringToInt( Value : String ) : Extended;"' );
  DoTest(@StringToIntFunc, '', 0, 'StringToInt('''')',false);
  DoTest(@StringToIntFunc, '1,000', 1000, 'StringToInt(''1,000'')',false);
  DoTest(@StringToIntFunc, '1.23', 1, 'StringToInt(''1.23'')',true);
  DoTest(@StringToIntFunc, '1234', 1234, 'StringToInt(''1234'')',false);
  DoTest(@StringToIntFunc, '1.2.3', 1, 'StringToInt(''1.2.3'')',true);
  DoTest(@StringToIntFunc, '3a', 1, 'StringToInt(''3a'')',true);
  Log('End of   "function StringToInt( Value : String ) : Extended;"' );
  Log('');
end;

procedure TForm11.IndentByStringTest;
begin
  Log('Start of "function  IndentBy( const Text : String; Count : Integer; BaseIndent : Integer = 4 ) : String;"' );
  DoTest(@IndentByStringFunc, 'ABC',0,0,'ABC','IndentBy(''ABC'',0)',false);
  DoTest(@IndentByStringFunc, 'ABC',1,0,'    ABC','IndentBy(''ABC'',1)',false);
  DoTest(@IndentByStringFunc, 'ABC',2,2,'    ABC','IndentBy(''ABC'',2,2)',false);
  Log('End of   "function  IndentBy( const Text : String; Count : Integer; BaseIndent : Integer = 4 ) : String;"' );
  Log('');
end;

procedure TForm11.IndentByFileTest;
begin
  Log('Start of "procedure IndentBy( var F : TextFile; Count : Integer; BaseIndent : Integer = 4 );"' );
  DoTest(@IndentByFileFunc, 0, 0, '', 'IndentBy( <file>, 0 )',false);
  DoTest(@IndentByFileFunc, 1, 0, '    ', 'IndentBy( <file>, 1 )',false);
  DoTest(@IndentByFileFunc, 1, 2, '  ', 'IndentBy( <file>, 1, 2 )',false);
  DoTest(@IndentByFileFunc, 2, 3, '      ', 'IndentBy( <file>, 2, 3 )',false);
  Log('End of   "procedure IndentBy( var F : TextFile; Count : Integer; BaseIndent : Integer = 4 );"' );
end;

function TForm11.EmptyTestFunc(Value: String): Boolean;
begin
  Result := Empty( Value );
end;

function TForm11.FloatToDollarsFunc(Value: Extended; theWidth: Integer): String;
begin
  if theWidth < 1 then
    Result := FloatToDollars( Value )
  else
    Result := FloatToDollars( Value, theWidth );
end;

function TForm11.FloatToPercentFunc(Value: Extended; theWidth: Integer): String;
begin
  if theWidth < 1 then
    Result := FloatToPercent( Value )
  else
    Result := FloatToPercent( Value, theWidth );
end;

function TForm11.DollarsToFloatFunc(Value: String): Extended;
begin
  Result := DollarsToFloat( Value );
end;

function TForm11.PercentToFloatFunc(Value: String): Extended;
begin
  Result := PercentToFloat( Value );
end;

function TForm11.StringToFloatFunc(Value: String): Extended;
begin
  Result := StringToFloat( Value );
end;

function TForm11.StringToIntFunc(Value: String): Integer;
begin
  Result := StringToInt( Value );
end;

function TForm11.IndentByStringFunc(Value: String; Count, BaseIndent: Integer
  ): String;
begin
  if BaseIndent < 1 then
    Result := IndentBy( Value, Count )
  else
    Result := IndentBy( Value, Count, BaseIndent );
end;

function TForm11.IndentByFileFunc(Count, BaseIndent: Integer): String;
function DefaultSaveLocation: string; // Copied from Common1
begin
  Result := GetAppConfigDir( False );
  Result := GetUserDir;
  //{$ifdef WIN32}
  //Result := Result + DirectorySeparator;
  //{$endif}
  Result := Result + ApplicationName + DirectorySeparator;
end;

var
F : Text;
S : String;
begin
S := DefaultSaveLocation;
ForceDirectories(S);
S := S + 'IndentBy.txt';
AssignFile( F, S );
Rewrite(F);
if BaseIndent < 1 then
  IndentBy( F, Count )
else
  IndentBy( F, Count, BaseIndent );
CloseFile(F);
Reset(F);
Readln(F,S);
CloseFile(F);
Result := S;
end;

procedure TForm11.RunSelectedTests;
begin
  inherited RunSelectedTests;
  if CheckGroup1.Checked[DollarsToFloatCI] then DollarsToFloatTest;
  if CheckGroup1.Checked[EmptyCI]          then EmptyTest;
  if CheckGroup1.Checked[FloatToDollarsCI] then FloatToDollarsTest;
  if CheckGroup1.Checked[FloatToPercentCI] then FloatToPercentTest;
  if CheckGroup1.Checked[PercentToFloatCI] then PercentToFloatTest;
  if CheckGroup1.Checked[StringToFloatCI]  then StringToFloatTest;
  if CheckGroup1.Checked[StringToIntCI]    then StringToIntTest;
  if CheckGroup1.Checked[IndentByStringCI] then IndentByStringTest;
  if CheckGroup1.Checked[IndentByFileCI]   then IndentByFileTest;
end;

end.

