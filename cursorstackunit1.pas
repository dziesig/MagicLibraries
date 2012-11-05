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

