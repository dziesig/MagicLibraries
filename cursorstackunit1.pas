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

unit CursorStackUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, generics1, Controls, Forms;

type

  TCursorStack = specialize TStack<TCursor>;

  { TFormCursorStack }

  TFormCursorStack = class(TCursorStack)
    private
      TheForm : TControl;
    public
      constructor Create( Form : TForm ); overload;
      destructor  Destroy; override;
      procedure Push( Value : TCursor ); override;
      function  Pop : TCursor; override;
  end;


implementation

{ TFormCursorStack }

constructor TFormCursorStack.Create(Form: TForm);
begin
  inherited Create;
  TheForm := Form;
end;

destructor TFormCursorStack.Destroy;
begin
  SetLength( Stack, 0 );
  inherited Destroy;
end;

procedure TFormCursorStack.Push(Value: TCursor);
begin
  inherited Push(TheForm.Cursor);
  TheForm.Cursor := Value;
  Application.ProcessMessages;
end;

function TFormCursorStack.Pop: TCursor;
begin
  Result:=inherited Pop;
  TheForm.Cursor := Result;
  Application.ProcessMessages;
end;

end.
