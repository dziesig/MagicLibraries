unit baseform2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Menus;

type

  { TForm1 }

  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  RunSelectedTests;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  SetCheckedItems( False );
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  SetCheckedItems( True );
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    begin
      Memo1.Lines.SaveToFile( SaveDialog1.FileName );
    end;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.SetCheckedItems(Value: Boolean);
var
  I : Integer;
begin
  for I := 0 to CheckGroup1.Items.Count - 1 do
    CheckGroup1.Checked[I] := Value;
end;

procedure TForm1.RunSelectedTests;
begin
  Memo1.Clear;
  Log( FormatDateTime( 'dddddd tt',Now ) );
  Log('');
end;

procedure TForm1.Log(Value: String; Err : Boolean );
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

