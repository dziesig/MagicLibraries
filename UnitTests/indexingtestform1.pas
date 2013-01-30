unit indexingtestform1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList,

  BaseForm2;

type


  TIndexTest        = function( Value  : Integer )  : String of object;

  { TIndexingTestForm }

  TIndexingTestForm = class(TBaseForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    procedure DoTest( Funct : TIndexTest; Argument : Integer;
                      ExpectedResult : String; Test : String;
                      ExceptionExpected : Boolean = False ); overload;


    function IndexBuildFunc( Value : Integer ) : String;
    function IndexNameFunc( Value : Integer ) : String;
    function IndexStepFunc( Value : Integer ) : String;
    function IndexIOFunc( Value : Integer ) : String;
    function IndexFindFunc( Value : Integer ) : String;

    procedure IndexBuildTest;
    procedure IndexNameTest;
    procedure IndexStepTest;
    procedure IndexIOTest;
    procedure IndexFindTest;
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
  IndexBuildCI              =  0;
  IndexNameCI               =  1;
  IndexStepCI               =  2;
  IndexIOCI                 =  3;
  IndexFindCI               =  4;


procedure TIndexingTestForm.DoTest(Funct: TIndexTest;
  Argument: Integer; ExpectedResult: String; Test: String;
  ExceptionExpected: Boolean);
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
    on E : Exception do
    if ExceptionExpected then
      Log(indentBy( Test + ' Passed', 2))
    else
      Log(IndentBy( Test + ' Failed.  Exception "' + E.Message + '" raised but not expected',2))
  end;


end;

procedure TIndexingTestForm.FormCreate(Sender: TObject);
begin
  CheckGroup1.Items.Add('Index Build');
  CheckGroup1.Items.Add('Index Name');
  CheckGroup1.Items.Add('Index Step');
  CheckGroup1.Items.Add('Index I/O');
  CheckGroup1.Items.Add('Index Find');
end;

function TIndexingTestForm.IndexBuildFunc(Value: Integer): String;
begin
  Result := IndexBuildingTest( Value );
end;

procedure TIndexingTestForm.IndexBuildTest;
begin
  Log('Start of Index Build Test' );
  DoTest( @IndexBuildFunc,0,'','6 Entry Build',False);
  Log('End   of Index Build Test' );
  Log('');
end;

function TIndexingTestForm.IndexFindFunc(Value: Integer): String;
begin
  Result := IndexFindingTest( Value )
end;

procedure TIndexingTestForm.IndexFindTest;
begin
  Log('Start of Index Find Test' );
  DoTest( @IndexFindFunc,0,'','6 Consecutive Entry Name and Id Find',False);
  DoTest( @IndexFindFunc,1,'','6 Consecutive Entry Name not present (higher) Find',True);
  DoTest( @IndexFindFunc,2,'','6 Consecutive Entry Name not present (interior) Find',False);
  DoTest( @IndexFindFunc,3,'','6 Consecutive Entry Name not present (lower) Find',False);
  DoTest( @IndexFindFunc,4,'','5 Entry Name Duplicates Find',False);  // Expects to find first entry
  Log('End   of Index Find Test' );
  Log('');
end;

function TIndexingTestForm.IndexIOFunc(Value: Integer): String;
begin
  Result := IndexInOutTest( Value );
end;

procedure TIndexingTestForm.IndexIOTest;
begin
  Log('Start of Index I/O Test' );
  DoTest( @IndexIOFunc,0,'','6 Entry Write/Read Name Index',False);
  Log('End   of Index I/O Test' );
  Log('');
end;

function TIndexingTestForm.IndexNameFunc(Value: Integer): String;
begin
  Result := IndexNamingTest( Value );
end;

procedure TIndexingTestForm.IndexNameTest;
begin
  Log('Start of Index Name Test' );
  DoTest( @IndexNameFunc,0,'','6 Entry Index Names',False);
  DoTest( @IndexNameFunc,1,'','6 Entry Invalid Index Name',True);
  Log('End   of Index Name Test' );
  Log('');
end;

function TIndexingTestForm.IndexStepFunc(Value: Integer): String;
begin
  Result := IndexStepingTest( Value );
end;

procedure TIndexingTestForm.IndexStepTest;
begin
  Log('Start of Index Step Test' );
  DoTest( @IndexStepFunc,0,'','Index Name Step',False);
  DoTest( @IndexStepFunc,1,'','Index Id Step',False);
  DoTest( @IndexStepFunc,2,'','Index Reverse Name Step',False);
  DoTest( @IndexStepFunc,3,'','Index Reverse Id Step',False);
  Log('End   of Index Step Test' );
  Log('');
end;

procedure TIndexingTestForm.RunSelectedTests;
begin
  inherited RunSelectedTests;
  if CheckGroup1.Checked[IndexBuildCI]     then IndexBuildTest;
  if CheckGroup1.Checked[IndexNameCI]      then IndexNameTest;
  if CheckGroup1.Checked[IndexStepCI]      then IndexStepTest;
  if CheckGroup1.Checked[IndexIOCI]        then IndexIOTest;
  if CheckGroup1.Checked[IndexFindCI]      then IndexFindTest;
end;

end.

