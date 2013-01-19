program Indexing;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, indexingtestform1, IndexingTestUnit1, baseform2, generics1, persists1,
  Common1, ObjectFactory1
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TIndexingTestForm, IndexingTestForm);
  Application.Run;
end.

