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

