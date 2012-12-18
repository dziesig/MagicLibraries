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

unit texttospeech1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

type

  { TTextToSpeech }

  TTextToSpeech = class(TProcess)
  private
    procedure SetText(AValue: String);
    public
      constructor Create;

      property Text : String write SetText;
  end;

implementation

{ TTextToSpeech }

procedure TTextToSpeech.SetText(AValue: String);
var
  Line : String;
  P : ^TTextToSpeech;
begin
  Line := '(SayText "' + AValue + '")';
  P := @self;
  Input.WriteBuffer( Line[1], Length(Line));
end;

constructor TTextToSpeech.Create;
begin
  inherited Create(nil);
  Options := [poUsePipes, poStderrToOutput, poNoConsole, poDefaultErrorMode];
  CommandLine := '/usr/bin/festival --pipe';
  Execute;
end;

end.

