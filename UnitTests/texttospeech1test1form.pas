unit texttospeech1test1form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  TextToSpeech1;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    TextToSpeech : TTextToSpeech;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TextToSpeech := TTextToSpeech.Create;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TextToSpeech.Text := 'Hello World';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TextToSpeech.Text := Edit1.Text;
end;

end.

