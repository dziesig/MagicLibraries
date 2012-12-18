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

unit DateTimeObjFrame1test1form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  datetimeframe1, datetimeobj1;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Frame1_1: TFrame1;
    Frame1_2: TFrame1;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    DateTime1 : TDateTimeObj;
    DateTime2 : TDateTimeObj;
    procedure Changed( Source : TObject );
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Frame1_1.MinCentury := 1800;
  Frame1_1.MaxCentury := 2000;
  Frame1_2.MinCentury := 1800;
  Frame1_2.MaxCentury := 2000;
  DateTime1 := nil;
  DateTime2 := nil;
  Frame1_1.OnChange := @Changed;
end;

procedure TForm1.Changed(Source: TObject);
begin
  if Assigned( Source ) then
    Label1.Caption := (Source as TDateTimeObj).AsString;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DateTime1.Free;
  DateTime1 := TDateTimeObj.Create( True, 1800 );
  Frame1_1.DateTimeObj := DateTime1;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  DateTime1.Free;
  DateTime1 := TDateTimeObj.Create( False, 1900, 3 );
  Frame1_1.DateTimeObj := DateTime1;
end;

end.
