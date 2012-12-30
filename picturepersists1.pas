unit PicturePersists1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  Graphics, Persists1, TextIO1;

type

  { TPicturePersists }

  TPicturePersists = class( TPersists )

  private
    fPixVal : String;

    procedure HexStringToByteArray( var A : array of Byte; S : String );

    function GetPicture: TPicture;
    procedure SetPicture(AValue: TPicture);
  public

    procedure MakeNew; override;
    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;

    property Picture : TPicture read GetPicture write SetPicture;
    property Pixval  : String   read fPixVal; // so we can test for changes
  end;

implementation

uses
  StrUtils, ObjectFactory1;

{ TPicturePersists }

const
  Version = 1;

procedure TPicturePersists.HexStringToByteArray(var A: array of Byte; S: String);
var
  Size : Integer;
  I    : Integer;
  B    : Byte;
  Q    : String;
begin
  Size := Length(S);
  for I := 0 to pred(Size div 2) do
    begin
      Q := Copy(S,I*2+1,2);
      B := Hex2Dec( Q );
      A[I] := B;
    end;
end;

function TPicturePersists.GetPicture: TPicture;
var
  Stream : TMemoryStream;
  Size   : Integer;
  P      : array of Byte;
begin
  Size := Length(fPixVal);
  if Size < 1 then
    begin
      Result := nil;
      exit;
    end;
  Result := TPicture.Create;
  Stream := TMemoryStream.Create;
  SetLength(P,Size div 2);  // This would be best done in HexStringToByteArray,
                            // but results in a compile-time error "invalid type"
  HexStringToByteArray( P, fPixVal );
  Stream.Seek(0,soFromBeginning);
  Stream.WriteBuffer( P[0], Size div 2 );
  Stream.Seek(0,soFromBeginning);
  Result.LoadFromStreamWithFileExt( Stream, 'jpg' );
  Stream.Free;
end;

procedure TPicturePersists.SetPicture(AValue: TPicture);
var
  Stream : TMemoryStream;
  P, A   : array of Byte;
  Size   : Integer;
  I      : Integer;
  QQQ    : TPicture;
begin
  fPixVal := '';
  Stream := TMemoryStream.Create;
  AValue.SaveToStreamWithFileExt( Stream, 'jpg' );
  Size := Stream.Size;
  Stream.Seek(0,soFromBeginning);

  SetLength( P, Size );
  Stream.ReadBuffer( P[0],Size);
  for I := 0 to pred(Size) do
    fPixVal := fPixVal + IntToHex(P[I],2);
{$ifdef CHECK_INTERNAL_DATA}
  SetLength( A, Size );
  HexStringToByteArray( A, fPixVal );
  for I := 0 to pred(Size) do
    if P[I] <> A[I] then
      MessageDlg('Mismatch at ' + IntToStr(I) + ' Got ' + IntToStr( A[I] ) + ' expected ' + IntToStr( P[I] ),
      mtConfirmation, [mbOk],0);
{$endif}
  Stream.Free;
end;

procedure TPicturePersists.MakeNew;
begin
  fPixVal := '';
  inherited MakeNew;
end;

procedure TPicturePersists.Save(TextIO: TTextIO);
begin
  SaveHeader( TextIO, Version );
  TextIO.WriteLn( fPixVal );
  UNMODIFY;
  SaveTrailer( TextIO );
end;

procedure TPicturePersists.Read(TextIO: TTextIO; Version: Integer);
begin
  MakeNew;
  if Version >= 1 then
    begin
      TextIO.Readln( fPixVal );
    end;
end;

initialization
  ObjectFactory.RegisterClass( TPicturePersists.ClassType );

end.

