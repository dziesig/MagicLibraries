unit Common1Test2Form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, baseform2,

  Common1, StdCtrls, ExtCtrls;

type

  { TForm11 }

  TForm11 = class(TForm1)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure DoTest( Funct : TColorSwapTest; Argument : Integer;
                      ExpectedResult : Integer; Test : String;
                      ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TCopyFileTest; Src, Dst : String; Test : String;
                      ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TCSSColorTest; Argument : Integer;
                      ExpectedResult : String; Test : String;
                      ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TExeNameTest; ExpectedResult : String;
                      Test : String;
                      ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct: TExtractFileOrDirectoryTest; Loc : Integer; Argument : String;
                      ExpectedResult : String; Test : String;
                      ExceptionExpected : Boolean = False );
    procedure DoTest( Funct : TMinMaxFloatTest; V0, V1 : Extended;
                      ExpectedResult : Extended; Test : String;
                      ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TMinMaxIntegerTest; V0, V1 : Integer;
                      ExpectedResult : Integer; Test : String;
                      ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct: TReadBoolTest; Loc : Integer; Argument : String;
                      ExpectedResult : Boolean; Test : String;
                      ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TRectToOriginTest; Value, ExpectedResult : TRect;
                      Test : String; ExceptionExpected : Boolean = false); overload;
    procedure DoTest( Funct : TStringToStringTest; Value, ExpectedResult : String;
                      Test : String; ExceptionExpected : Boolean = False ); overload;
    procedure DoTest( Funct : TSetPositionCBTest; Value : String;
                      ExpectedResult : Integer; Test : String;
                      ExceptionExpected : Boolean = false ); overload;
    procedure DoTest( Funct : TSetPositionRGTest; Value : String;
                      ExpectedResult : Integer; Test : String;
                      ExceptionExpected : Boolean = false ); overload;

    procedure ColorSwapTest;
    procedure CopyFileTest;
    procedure CSSColorTest;
    procedure ExeNameTest;
    procedure ExtractFileOrDirectoryTest;
    procedure MaxFloatTest;
    procedure MaxIntegerTest;
    procedure MinFloatTest;
    procedure MinIntegerTest;
    procedure ReadBoolTest;
    procedure RectToOriginTest;
    procedure RemoveExtTest;
    procedure SetPositionCBTest;
    procedure SetPositionRGTest;
    procedure WalkDirectoryTreeTest;
    procedure WalkDirectoryTreeObjTest;

    function ColorSwapFunc( Value : Integer ) : Integer;
    function CopyFileFunc( Src, Dst : String ) : Boolean;
    function CSSColorFunc( Value : Integer ) : String;
    function ExeNameFunc : String;
    function ExtractFileOrDirectoryFunc( Index : Integer; Value : String ) : String;
    function MaxFloatFunc( V0, V1 : Extended ) : Extended;
    function MaxIntegerFunc( V0, V1 : Integer ) : Integer;
    function MinFloatFunc( V0, V1 : Extended ) : Extended;
    function MinIntegerFunc(  V0, V1 : Integer  ) : Integer;
    function ReadBoolFunc(  ) : Boolean;
    function RectToOriginFunc( Value : TRect ) : TRect;
    function RemoveExtFunc( Value : String ) : String;
    function SetPositionCBFunc( Value : TComboBox; Item : String ) : Integer;
    function SetPositionRGFunc( Value : TRadioGroup; Item : String ) : Integer;
    //function WalkDirectoryTreeFunc(  ) : ;
    //function WalkDirectoryTreeObjFunc(  ) : ;

    function  CompareTestFiles( Src, Dst : String ) : Boolean;

  public
    { public declarations }
    procedure RunSelectedTests; override;
  end;

var
  Form11: TForm11;

implementation

{$R *.lfm}

uses
  StringSubs;

{ TForm11 }

{ The following must correspond to the order of check items in CheckGroup1     }

const
  ColorSwapCI              =  0;
  CopyFileCI               =  1;
  CSSColorCI               =  2;
  ExeNameCI                =  3;
  ExtractFileOrDirectoryCI =  4;
  MaxFloatCI               =  5;
  MaxIntegerCI             =  6;
  MinFloatCI               =  7;
  MinIntegerCI             =  8;
  ReadBoolCI               =  9;
  RectToOriginCI           = 10;
  RemoveExtCI              = 11;
  SetPositionCBCI          = 12;
  SetPositionRGCI          = 13;
  WalkDirectoryTreeCI      = 14;
  WalkDirectoryTreeObjCI   = 15;

procedure TForm11.FormCreate(Sender: TObject);
begin
  Caption := 'Common1 Unit Test';
  CheckGroup1.Items.Add('ColorSwap');
  CheckGroup1.Items.Add('CopyFile');
  CheckGroup1.Items.Add('CSSColor');
  CheckGroup1.Items.Add('ExeName');
  CheckGroup1.Items.Add('ExtractFileOrDirectory');
  CheckGroup1.Items.Add('Max (Float)');
  CheckGroup1.Items.Add('Max (Integer)');
  CheckGroup1.Items.Add('Min (Float)');
  CheckGroup1.Items.Add('Min (Integer)');
  CheckGroup1.Items.Add('ReadBool');
  CheckGroup1.Items.Add('RectToOrigin');
  CheckGroup1.Items.Add('RemoveExt');
  CheckGroup1.Items.Add('SetPositionCB');
  CheckGroup1.Items.Add('SetPositionRG');
  CheckGroup1.Items.Add('WalkDirectoryTree');
  CheckGroup1.Items.Add('WalkDirectoryTree (obj)');
end;

procedure TForm11.DoTest(Funct: TColorSwapTest; Argument: Integer;
  ExpectedResult: Integer; Test: String; ExceptionExpected: Boolean);
var
  Result : Integer;
begin
  try
    Result := Funct( Argument);
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.  Expected:  "' +
             IntToHex(ExpectedResult, 6) + '", got:  "' + IntToHex(Result,6) +
             '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TCopyFileTest; Src, Dst: String; Test: String;
  ExceptionExpected: Boolean);
var
  Result : Boolean;
begin
  try
    Result := Funct( Src, Dst );
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2), true)
    else
      if Result then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.  Expected:  " True, got:' + BoolToStr( Result ), 2),true);

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TCSSColorTest; Argument: Integer;
  ExpectedResult: String; Test: String; ExceptionExpected: Boolean);
var
  Result : String;
begin
  try
    Result := Funct( Argument);
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.  Expected:  "' +
             ExpectedResult + '", got:  "' + Result +
             '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TExeNameTest; ExpectedResult: String;
  Test: String; ExceptionExpected: Boolean);
var
  Result : String;
begin
  try
    Result := Funct();
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.  Expected:  "' +
             ExpectedResult + '", got:  "' + Result +
             '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TExtractFileOrDirectoryTest; Loc: Integer;
  Argument: String; ExpectedResult: String; Test: String;
  ExceptionExpected: Boolean);
var
  Result : String;
begin
  try
    Result := Funct(Loc, Argument);
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.  Expected:  "' +
             ExpectedResult + '", got:  "' + Result +
             '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TMinMaxFloatTest; V0, V1: Extended;
  ExpectedResult: Extended; Test: String; ExceptionExpected: Boolean);
var
  Result : Extended;
begin
  try
    Result := Funct( V0, V1 );
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.  Expected:  "' +
             FloatToStr(ExpectedResult) + '", got:  "' + FloatToStr(Result) +
             '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TMinMaxIntegerTest; V0, V1: Integer;
  ExpectedResult: Integer; Test: String; ExceptionExpected: Boolean);
var
  Result : Extended;
begin
  try
    Result := Funct( V0, V1 );
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.  Expected:  "' +
             FloatToStr(ExpectedResult) + '", got:  "' + FloatToStr(Result) +
             '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TReadBoolTest; Loc: Integer; Argument: String;
  ExpectedResult: Boolean; Test: String; ExceptionExpected: Boolean);
begin

end;

procedure TForm11.DoTest(Funct: TRectToOriginTest; Value,
  ExpectedResult: TRect; Test: String; ExceptionExpected: Boolean);
var
  Result : TRect;
begin
  try
    Result := Funct( Value );
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Equal(Result, ExpectedResult) then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.',2), True);
        //Log( IndentBy( Test + ' Failed.  Expected:  "' +
        //     FloatToStr(ExpectedResult) + '", got:  "' + FloatToStr(Result) +
        //     '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TStringToStringTest; Value,
  ExpectedResult: String; Test: String; ExceptionExpected: Boolean);
var
  Result : String;
begin
  try
    Result := Funct( Value );
    if ExceptionExpected then
      Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
    else
      if Result = ExpectedResult then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log( IndentBy( Test + ' Failed.  Expected:  "' +
             ExpectedResult + '", got:  "' + Result +
             '"',2), True)

  except
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
  end;

end;

procedure TForm11.DoTest(Funct: TSetPositionCBTest; Value: String;
  ExpectedResult: Integer; Test: String; ExceptionExpected: Boolean);
var
  Result : Integer;
  CB     : TComboBox;
begin
  CB := TComboBox.Create( nil );
  CB.Items.Add('One');
  CB.Items.Add('Two');
  CB.Items.Add('Three');
  try
    try
      Result := Funct( CB, Value);
      if ExceptionExpected then
        Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
      else
        if Result = ExpectedResult then
          Log(indentBy( Test + ' Passed', 2))
        else
          Log( IndentBy( Test + ' Failed.  Expected:  "' +
               IntToHex(ExpectedResult, 6) + '", got:  "' + IntToHex(Result,6) +
               '"',2), True)

    except
      if ExceptionExpected then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
    end;
  finally
    CB.Free;
  end;
end;

procedure TForm11.DoTest(Funct: TSetPositionRGTest; Value: String;
  ExpectedResult: Integer; Test: String; ExceptionExpected: Boolean);
var
  Result : Integer;
  RG     : TRadioGroup;
begin
  RG := TRadioGroup.Create( nil );
  RG.Items.Add('One');
  RG.Items.Add('Two');
  RG.Items.Add('Three');
  try
    try
      Result := Funct( RG, Value);
      if ExceptionExpected then
        Log(IndentBy( Test + ' Failed.  Exception expected but not raised',2))
      else
        if Result = ExpectedResult then
          Log(indentBy( Test + ' Passed', 2))
        else
          Log( IndentBy( Test + ' Failed.  Expected:  "' +
               IntToHex(ExpectedResult, 6) + '", got:  "' + IntToHex(Result,6) +
               '"',2), True)

    except
      if ExceptionExpected then
        Log(indentBy( Test + ' Passed', 2))
      else
        Log(IndentBy( Test + ' Failed.  Exception raised but not expected',2))
    end;
  finally
    RG.Free;
  end;
end;

procedure TForm11.ColorSwapTest;
begin
  Log('Start of "function ColorSwap(Value: Integer): Integer;"' );
  DoTest(@ColorSwapFunc, $0000ff, $ff0000, 'ColorSwap( $0000ff )',false);
  DoTest(@ColorSwapFunc, $00ff00, $00ff00, 'ColorSwap( $00ff00 )',false);
  DoTest(@ColorSwapFunc, $ff0000, $0000ff, 'ColorSwap( $ff0000 )',false);
  Log('End of   "function ColorSwap(Value: Integer): Integer;"' );
  Log('');
end;

procedure TForm11.CopyFileTest;
var
  SrcFile, DstFile : String;
  TestPath : String;
begin
  Log('Start of "procedure CopyFile(Src, Dst : String);"' );
  TestPath := DefaultSaveLocation + DirectorySeparator + 'CopyFileTest' +
              DirectorySeparator;
  if not DirectoryExists( TestPath ) then
    ForceDirectories( TestPath );
  SrcFile := ExePath + 'TestFile.txt';
  DstFile := TestPath + 'TestFile.txt';
  DoTest(@CopyFileFunc, SrcFile, DstFile, 'CopyFile( ' + SrcFile +', ' + DstFile + ' )',false );
  Log('End of   "procedure CopyFile(Src, Dst : String);"' );
  Log('');
end;

procedure TForm11.CSSColorTest;
begin
  Log('Start of "function CSSColor(Value: Integer): String;"' );
  DoTest(@CSSColorFunc, $ffffff, 'FFFFFF', 'CSSColor($fffff)',false);
  DoTest(@CSSColorFunc, $012345, '012345', 'CSSColor($01234)',false);
  Log('End of   "function CSSColor(Value: Integer): String;"' );
  Log('');
end;

procedure TForm11.ExeNameTest;
begin
  Log('Start of "function ExeName : String;"' );
  DoTest(@ExeNameFunc,'Common1Test1','ExePath', false );
  Log('End of   "function ExeName : String;"' );
  Log('');
end;

procedure TForm11.ExtractFileOrDirectoryTest;
const
{$ifdef WIN32}
  TestLine = '\home\me\desktop\test.txt';
{$else}
  TestLine = '/home/me/desktop/test.txt';
{$endif}
begin
  Log('Start of "function ExtractFileOrDirectory( Loc : Integer; Path : String ) : String;"' );
  DoTest(@ExtractFileOrDirectoryFunc, -1, TestLine, 'test.txt','ExtractFileOrDirectory',false);
  DoTest(@ExtractFileOrDirectoryFunc,  0, TestLine, 'home','ExtractFileOrDirectory',false);
  DoTest(@ExtractFileOrDirectoryFunc,  1, TestLine, 'me','ExtractFileOrDirectory',false);
  DoTest(@ExtractFileOrDirectoryFunc,  2, TestLine, 'desktop','ExtractFileOrDirectory',false);
  Log('End of   "function ExtractFileOrDirectory( Loc : Integer; Path : String ) : String;"' );
  Log('');
end;

procedure TForm11.MaxFloatTest;
begin
  Log('Start of "function Max( V0, V1 : Extended ) : Extended;"' );
  DoTest(@MaxFloatFunc,  0.0,  0.0,  0.0, 'Max( 0.0, 0.0)',false);
  DoTest(@MaxFloatFunc,  0.0,  1.0,  1.0, 'Max( 0.0, 1.0)',false);
  DoTest(@MaxFloatFunc,  0.0, -1.0,  0.0, 'Max( 0.0, 0.0)',false);
  DoTest(@MaxFloatFunc,  1.0,  0.0,  1.0, 'Max( 1.0, 0.0)',false);
  DoTest(@MaxFloatFunc, -1.0,  0.0,  0.0, 'Max(-1.0, 0.0)',false);
  DoTest(@MaxFloatFunc, -1.0, -2.0, -1.0, 'Max(-1.0,-2.0)',false);
  DoTest(@MaxFloatFunc, -2.0, -1.0, -1.0, 'Max(-2.0,-1.0)',false);
  Log('End of   "function Max( V0, V1 : Extended ) : Extended;"' );
  Log('');
end;

procedure TForm11.MaxIntegerTest;
begin
  Log('Start of "function Max( V0, V1 : Integer ) : Integer;"' );
  DoTest(@MaxIntegerFunc,  0,  0,  0, 'Max( 0, 0)',false);
  DoTest(@MaxIntegerFunc,  0,  1,  1, 'Max( 0, 1)',false);
  DoTest(@MaxIntegerFunc,  0, -1,  0, 'Max( 0, 0)',false);
  DoTest(@MaxIntegerFunc,  1,  0,  1, 'Max( 1, 0)',false);
  DoTest(@MaxIntegerFunc, -1,  0,  0, 'Max(-1, 0)',false);
  DoTest(@MaxIntegerFunc, -1, -2, -1, 'Max(-1,-2)',false);
  DoTest(@MaxIntegerFunc, -2, -1, -1, 'Max(-2,-1)',false);
  Log('End of   "function Max( V0, V1 : Integer ) : Integer;"' );
  Log('');
end;

procedure TForm11.MinFloatTest;
begin
  Log('Start of "function Min( V0, V1 : Extended ) : Extended;"' );
  DoTest(@MinFloatFunc,  0.0,  0.0,  0.0, 'Min( 0.0, 0.0)',false);
  DoTest(@MinFloatFunc,  0.0,  1.0,  0.0, 'Min( 0.0, 1.0)',false);
  DoTest(@MinFloatFunc,  0.0, -1.0, -1.0, 'Min( 0.0, 0.0)',false);
  DoTest(@MinFloatFunc,  1.0,  0.0,  0.0, 'Min( 1.0, 0.0)',false);
  DoTest(@MinFloatFunc, -1.0,  0.0, -1.0, 'Min(-1.0, 0.0)',false);
  DoTest(@MinFloatFunc, -1.0, -2.0, -2.0, 'Min(-1.0,-2.0)',false);
  DoTest(@MinFloatFunc, -2.0, -1.0, -2.0, 'Min(-2.0,-1.0)',false);
  Log('End of   "function Min( V0, V1 : Extended ) : Extended;"' );
  Log('');
end;

procedure TForm11.MinIntegerTest;
begin
  Log('Start of "function Min( V0, V1 : Integer ) : Integer;"' );
  DoTest(@MinIntegerFunc,  0,  0,  0, 'Min( 0, 0)',false);
  DoTest(@MinIntegerFunc,  0,  1,  0, 'Min( 0, 1)',false);
  DoTest(@MinIntegerFunc,  0, -1, -1, 'Min( 0, 0)',false);
  DoTest(@MinIntegerFunc,  1,  0,  0, 'Min( 1, 0)',false);
  DoTest(@MinIntegerFunc, -1,  0, -1, 'Min(-1, 0)',false);
  DoTest(@MinIntegerFunc, -1, -2, -2, 'Min(-1,-2)',false);
  DoTest(@MinIntegerFunc, -2, -1, -2, 'Min(-2,-1)',false);
  Log('End of   "function Min( V0, V1 : Integer ) : Integer;"' );
  Log('');
end;

procedure TForm11.ReadBoolTest;
begin
  Log( 'ReadBoolTest not yet implemented');
  Log('');
end;

procedure TForm11.RectToOriginTest;
var
  R1, R2 : TRect;
  function MakeRect( Left, Top, Right, Bottom : Integer ) : TRect;
  begin
    Result.Left    := Left;
    Result.Top     := Top;
    Result.Right   := Right;
    Result.Bottom  := Bottom;
  end;

begin
  R1 := MakeRect( 100,200,200,400 );
  R2 := MakeRect(   0,  0,100,200 );
  Log('Start of "function RectToOrigin( R0, R1 : TRect ) : TRect;"' );
  DoTest(@RectToOriginFunc, R1, R2, 'RectToOrigin( R1, R2 )');
  Log('End of   "function RectToOrigin( R0, R1 : TRect ) : TRect;"' );
  Log('');
end;

procedure TForm11.RemoveExtTest;
begin
  Log('Start of "function RemoveExt( Value : String) : String;"' );
  DoTest(@RemoveExtFunc, 'myfile.txt', 'myfile', 'RemoveExt(''myfile.txt'')' );
  DoTest(@RemoveExtFunc, 'myfile', 'myfile', 'RemoveExt(''myfile'')'  );
  DoTest(@RemoveExtFunc, '/home/me/myfile.txt', '/home/me/myfile', 'RemoveExt(''/home/me/myfile.txt'')'  );
  Log('End of   "function RemoveExt( Value : String) : String;"' );
  Log('');
end;

procedure TForm11.SetPositionCBTest;
begin
  Log('Start of "function SetPositionCB( CB : TComboBox; Value : String) : Integer;"' );
  DoTest(@SetPositionCBFunc, 'One', 0, 'SetPositionRG( ''One'' )' );
  DoTest(@SetPositionCBFunc, 'Two', 1, 'SetPositionRG( ''Two'' )' );
  DoTest(@SetPositionCBFunc, 'Three', 2, 'SetPositionRG( ''Three'' )' );
  DoTest(@SetPositionCBFunc, 'Four', -1, 'SetPositionRG( ''Four'' )' );
  Log('Start of "function SetPositionCB(  : TComboBox; Value : String) : Integer;"' );
  Log('');
end;

procedure TForm11.SetPositionRGTest;
begin
  Log('Start of "function SetPositionRG( RG : TRadioGroup; Value : String) : Integer;"' );
  DoTest(@SetPositionRGFunc, 'One', 0, 'SetPositionRG( ''One'' )' );
  DoTest(@SetPositionRGFunc, 'Two', 1, 'SetPositionRG( ''Two'' )' );
  DoTest(@SetPositionRGFunc, 'Three', 2, 'SetPositionRG( ''Three'' )' );
  DoTest(@SetPositionRGFunc, 'Four', -1, 'SetPositionRG( ''Four'' )' );
  Log('Start of "function SetPositionRG( RG : TRadioGroup; Value : String) : Integer;"' );
  Log('');
end;

procedure TForm11.WalkDirectoryTreeTest;
begin
  Log( 'WalkDirectoryTree not tested' );
  Log('');
end;

procedure TForm11.WalkDirectoryTreeObjTest;
begin
  Log( 'WalkDirectoryTree of object not tested' );
  Log('');
end;

function TForm11.ColorSwapFunc(Value: Integer): Integer;
begin
  Result := ColorSwap( Value );
end;

function TForm11.CopyFileFunc(Src, Dst: String): Boolean;
begin
  CopyFile( Src, Dst );
  Result := CompareTestFiles( Src, Dst );
//  if Result then DeleteFile(Dst);   // Don't delete if failed
end;

function TForm11.CSSColorFunc(Value: Integer): String;
begin
  Result := CSSColor( Value );
end;

function TForm11.ExeNameFunc: String;
begin
  Result := ExeName;
end;

function TForm11.ExtractFileOrDirectoryFunc( Index: Integer; Value: String
  ): String;
begin
  Result := ExtractFileOrDirectory( Index, Value );
end;

function TForm11.MaxFloatFunc(V0, V1: Extended): Extended;
begin
  Result := Max(V0,V1);
end;

function TForm11.MaxIntegerFunc(V0, V1: Integer): Integer;
begin
  Result := Max(V0,V1);
end;

function TForm11.MinFloatFunc(V0, V1: Extended): Extended;
begin
  Result := Min(V0,V1);
end;

function TForm11.MinIntegerFunc(V0, V1: Integer): Integer;
begin
  Result := Min(V0,V1);
end;

function TForm11.ReadBoolFunc: Boolean;
begin

end;

function TForm11.RectToOriginFunc(Value: TRect): TRect;
begin
  Result := RectToOrigin( Value );
end;

function TForm11.RemoveExtFunc(Value: String): String;
begin
  Result := RemoveExt( Value );
end;

function TForm11.SetPositionCBFunc(Value: TComboBox; Item: String): Integer;
begin
  SetPositionCB( Value, Item );
  Result := Value.ItemIndex;
end;

function TForm11.SetPositionRGFunc(Value: TRadioGroup; Item: String): Integer;
begin
  SetPositionRG( Value, Item );
  Result := Value.ItemIndex;
end;

function TForm11.CompareTestFiles(Src, Dst: String): Boolean;
var
  FSrc, FDst : Text;
  SrcLine, DstLine : String;
begin
  Result := true;
  AssignFile( FSrc, Src );
  Reset(FSrc);
  AssignFile( FDst, Dst );
  Reset( FDst );
  while not Eof(FSrc) do
    begin
      Readln(FSrc, SrcLine);
      Readln(FDst, DstLine);
      if SrcLine <> DstLine then Result := False
    end;
  if not Eof(FDst) then Result := False;
  CloseFile( FSrc );
  CloseFile( FDst );
end;

procedure TForm11.RunSelectedTests;
begin
  inherited RunSelectedTests;
  if CheckGroup1.Checked[ColorSwapCI]              then  ColorSwapTest;
  if CheckGroup1.Checked[CopyFileCI]               then  CopyFileTest;
  if CheckGroup1.Checked[CSSColorCI]               then  CSSColorTest;
  if CheckGroup1.Checked[ExeNameCI]                then  ExeNameTest;
  if CheckGroup1.Checked[ExtractFileOrDirectoryCI] then  ExtractFileOrDirectoryTest;
  if CheckGroup1.Checked[MaxFloatCI]               then  MaxFloatTest;
  if CheckGroup1.Checked[MaxIntegerCI]             then  MaxIntegerTest;
  if CheckGroup1.Checked[MinFloatCI]               then  MinFloatTest;
  if CheckGroup1.Checked[MinIntegerCI]             then  MinIntegerTest;
  if CheckGroup1.Checked[ReadBoolCI]               then  ReadBoolTest;
  if CheckGroup1.Checked[RectToOriginCI]           then RectToOriginTest;
  if CheckGroup1.Checked[RemoveExtCI]              then RemoveExtTest;
  if CheckGroup1.Checked[SetPositionCBCI]          then SetPositionCBTest;
  if CheckGroup1.Checked[SetPositionRGCI]          then SetPositionRGTest;
  if CheckGroup1.Checked[WalkDirectoryTreeCI]      then WalkDirectoryTreeTest;
  if CheckGroup1.Checked[WalkDirectoryTreeObjCI]   then WalkDirectoryTreeObjTest;
end;

end.

