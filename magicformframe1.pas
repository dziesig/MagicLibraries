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

{==============================================================================}
{ The MagicFormFrame is a frame that can host multiple Forms (from TForm )     }
{ such that the user interface can be limited to a single window but the code  }
{ (and components) can be separated into individual files (so you don't have a }
{ single humongous file defining the user interface.                           }
{==============================================================================}

unit MagicFormFrame1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
  private
    { private declarations }
    FForm: TForm;
    FOnAfterFormChanged: TNotifyEvent;
    FOnBeforeFormChanged: TNotifyEvent;
    procedure SetForm(AValue: TForm);
  protected
    { protected declarations }
    procedure AfterFormChanged; virtual;
    procedure BeforeFormChanged; virtual;

    procedure Paint; override;
    procedure Resize; override;
  public
    { public declarations }
    property Form: TForm read FForm write SetForm;

    property OnAfterFormChanged  : TNotifyEvent read FOnAfterFormChanged  write FOnAfterFormChanged;
    property OnBeforeFormChanged : TNotifyEvent read FOnBeforeFormChanged write FOnBeforeFormChanged;
  end;

implementation

{$R *.lfm}

{ TFrame1 }

procedure TFrame1.SetForm(AValue: TForm);
var
  I : Integer;
begin
  if AValue <> FForm then
  begin
    BeforeFormChanged;
    if (FForm <> nil) then { hide current form }
      FForm.Visible := False;
    FForm := AValue;
    if (FForm <> nil) then
      begin
        FForm.Parent := Self;
        //FForm.BorderIcons := [];
        //FForm.BorderStyle := bsNone; { this will change AutoScroll }
//        FForm.AutoScroll := True;  { re-assign after changing BorderStyle }
//        FForm.AutoSize := False;
//        FForm.Position := poDesigned;
//        Resize;
        FForm.Visible := True;
      end;
    AfterFormChanged;
  end;
end;

procedure TFrame1.AfterFormChanged;
begin
  if Assigned(FOnAfterFormChanged) then FOnAfterFormChanged(Self);
end;

procedure TFrame1.BeforeFormChanged;
begin
  if Assigned(FOnBeforeFormChanged) then FOnBeforeFormChanged(Self);
end;

procedure TFrame1.Paint;
var
  NewForm: TForm;
  FormClassName: String;
  I: Integer;
begin
  inherited Paint;
  if FForm <> nil then
    SetForm(FForm);
end;

procedure TFrame1.Resize;
var
  Rect0, Rect1 : TRect;
  PWidth, PHeight : Integer;
begin
  inherited;
  if (FForm <> nil) and not (csDestroying in ComponentState) then
    begin
      //FForm.GetPreferredSize( PWidth, PHeight, True );
      //FForm.SetBounds(0, 0, PWidth, PHeight);
    end;
end;

end.

