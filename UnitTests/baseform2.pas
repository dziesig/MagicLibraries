unit baseform2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Menus;

type

  { TBaseForm }

  TBaseForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CheckGroup1: TCheckGroup;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    { private declarations }
    procedure SetCheckedItems( Value : Boolean );
  public
    { public declarations }
    procedure RunSelectedTests; virtual;
    procedure Log( Value : String; Err : Boolean = false );
  end;

var
  BaseForm: TBaseForm;

implementation

{$R *.lfm}

{ TBaseForm }

procedure TBaseForm.BitBtn1Click(Sender: TObject);
begin
  RunSelectedTests;
end;

procedure TBaseForm.BitBtn2Click(Sender: TObject);
begin
  SetCheckedItems( False );
end;

procedure TBaseForm.BitBtn3Click(Sender: TObject);
begin
  SetCheckedItems( True );
end;

procedure TBaseForm.MenuItem2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    begin
      Memo1.Lines.SaveToFile( SaveDialog1.FileName );
    end;
end;

procedure TBaseForm.MenuItem4Click(Sender: TObject);
begin
  Close;
end;

procedure TBaseForm.SetCheckedItems(Value: Boolean);
var
  I : Integer;
begin
  for I := 0 to CheckGroup1.Items.Count - 1 do
    CheckGroup1.Checked[I] := Value;
end;

procedure TBaseForm.RunSelectedTests;
begin
  Memo1.Clear;
  Log( FormatDateTime( 'dddddd tt',Now ) );
  Log('');
end;

procedure TBaseForm.Log(Value: String; Err : Boolean );
begin
  if Err then
    begin
      Memo1.Font.Style := Memo1.Font.Style + [fsBold];
      Memo1.Font.Color := clRed;
    end
  else
    begin
      Memo1.Font.Style := Memo1.Font.Style - [fsBold];
    end;
  Application.ProcessMessages;
  Memo1.Lines.Add( Value );
end;

end.

