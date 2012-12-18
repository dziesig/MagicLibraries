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

