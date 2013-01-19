unit indexingtestform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList,

  BaseForm2;

type


  TNameOnlyIndexTest        = function( Value  : Integer )  : Integer of object;

  { TIndexingTestForm }

  TIndexingTestForm = class(TBaseForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure DoTest( Funct : TNameOnlyIndexTest; Argument : Integer;
                      ExpectedResult : Integer; Test : String;
                      ExceptionExpected : Boolean = False ); overload;

    function NameOnlyIndexingFunc( Value : Integer ) : Integer;

    procedure NameOnlyIndexingTest;
  public
    { public declarations }
    procedure RunSelectedTests; override;
  end;

var
  IndexingTestForm: TIndexingTestForm;

implementation

{$R *.lfm}

uses
  Stringsubs, IndexingTestUnit1;

{ TIndexingTestForm }

{ The following must correspond to the order of check items in CheckGroup1     }

const
  NameOnlyIndexingCI              =  0;

procedure TIndexingTestForm.DoTest(Funct: TNameOnlyIndexTest;
  Argument: Integer; ExpectedResult: Integer; Test: String;
  ExceptionExpected: Boolean);
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

procedure TIndexingTestForm.FormCreate(Sender: TObject);
begin
  CheckGroup1.Items.Add('Name Only Indexing');

end;

function TIndexingTestForm.NameOnlyIndexingFunc(Value: Integer): Integer;
begin
  // The Indexing Test Unit raises exceptions if test fails.
  // Passing Tests return normally
  NameOnlyIndexTest( Value );
  Result := 0;
end;

procedure TIndexingTestForm.NameOnlyIndexingTest;
begin
  Log('Start of Name Only Indexing Test' );
  DoTest( @NameOnlyIndexingFunc,0,0,'NameOnlyIndexingTest(0)',False);
  DoTest( @NameOnlyIndexingFunc,1,0,'NameOnlyIndexingTest(1)',False);
  DoTest( @NameOnlyIndexingFunc,2,0,'NameOnlyIndexingTest(2)',False);
  DoTest( @NameOnlyIndexingFunc,3,0,'NameOnlyIndexingTest(3)',False);
  Log('End   of Name Only Indexing Test' );
  Log('');
end;

procedure TIndexingTestForm.RunSelectedTests;
begin
  inherited RunSelectedTests;
  if CheckGroup1.Checked[NameOnlyIndexingCI]     then NameOnlyIndexingTest;
end;

end.

