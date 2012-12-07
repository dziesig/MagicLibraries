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


unit DateTimeObj1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Persists1, TextIO1;

type

  TYear = 1..65535;
  TMonth = 1..12;
  TDay =1..31;
  THour = 0..23;
  TMinute = 0..59;
  TSecond = 0..59;

  { TDateTimeObj }

  TDateTimeObj = class( TPersists )
  private
    fAMPM : Boolean;
    Y, M, D, H, Mi, S, U : Word;
    function GetDate: TDateTime;
    function GetDay: TDay;
    function GetHour: THour;
    function GetMin: TMinute;
    function GetMonth: TMonth;
    function GetPM: Boolean;
    function GetSecond: TSecond;
    function GetTime: TDateTime;
    function GetYear: TYear;
    procedure SetDay(AValue: TDay);
    procedure SetHour(AValue: THour);
    procedure SetMin(AValue: TMinute);
    procedure SetMonth(AValue: TMonth);
    procedure SetPM(AValue: Boolean);
    procedure SetSecond(AValue: TSecond);
    procedure SetYear(AValue: TYear);
  public
    DateTime : TDateTime;

    constructor Create( IsAMPM : Boolean; aParent : TPersists = nil); overload;
    constructor Copy( Src : TDateTimeObj );

    procedure MakeNew; override;
    procedure Save( TextIO : TTextIO ); override;
    procedure Read( TextIO : TTextIO; Version : Integer ); override;

    property Date : TDateTime read GetDate;
    property Time : TDateTime read GetTime;

    procedure AddSeconds( Seconds : Double );
    procedure AddMinutes( Minutes : Double );

    property Year    : TYear    read GetYear    write SetYear;
    property Month   : TMonth   read GetMonth   write SetMonth;
    property Day     : TDay     read GetDay     write SetDay;
    property Hour    : THour    read GetHour    write SetHour;
    property Min     : TMinute  read GetMin     write SetMin;
    property Second  : TSecond  read GetSecond  write SetSecond;
    property PM      : Boolean  read GetPM      write SetPM;
    property AMPM    : Boolean  read fAMPM;
  end;

  {------------------------------------------------------------------------------}
  { Function type definitions for UnitTest driver program                        }
  {------------------------------------------------------------------------------}

  type
    TTimeTest            = function( Hour : THour; Minute : TMinute; AMPM, PM : Boolean ) : Word of object;
    TMonthLengthTest     = function( Month : TMonth; Day : TDay )  : Word of object;
    TLeapYearTest        = function( Year : TYear; Day  : TDay )  : Word of object;
    TDateTimeObjIOTest   = function( FileName : String; Year : TYear;
                                     Month : TMonth; Day : TDay; Hour : THour;
                                     Minute : TMinute;
                                     AMPM, PM : Boolean ) : Word of object;
    TAddSecAndMinTest    = function( Hour : THour; Minute : TMinute; AddValue : Word;
                                     UseMinutes : Boolean ) : Word of object;

implementation

uses
  DateUtils,
  ObjectFactory1;

{ TDateTimeObj }

const
  Version = 1;

function TDateTimeObj.GetDate: TDateTime;
begin
  Result := Double(trunc(DateTime));
end;

function TDateTimeObj.GetDay: TDay;
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  Result := D;
end;

function TDateTimeObj.GetHour: THour;
begin
  DecodeDateTime(DateTime, Y, M, D, H, Mi, S, U );
  if AMPM then
    begin
      Result := H mod 12;
      if Result = 0 then
        Result := 12;
    end
  else
    begin
      Result := H;
    end;

end;

function TDateTimeObj.GetMin: TMinute;
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  Result := Mi;
end;

function TDateTimeObj.GetMonth: TMonth;
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  Result := M;
end;

function TDateTimeObj.GetPM: Boolean;
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  Result := H >= 12;
end;

function TDateTimeObj.GetSecond: TSecond;
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  Result := S;
end;

function TDateTimeObj.GetTime: TDateTime;
begin
  Result := DateTime - Date;
end;

function TDateTimeObj.GetYear: TYear;
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  Result := Y;
end;

procedure TDateTimeObj.SetDay(AValue: TDay);
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  D := AValue;
  DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
end;

procedure TDateTimeObj.SetHour(AValue: THour);
var
  HR : Word;
begin
  HR := AValue;
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  if AMPM then
    begin
      if ( HR < 1 ) or ( HR > 12 ) then
        raise ERangeError.Create( IntToStr( HR ) + ' is not a valid AM/PM hour.');
      if PM then
        begin
          if HR = 12 then
            HR := 13;
        end
      else
        begin
          if HR = 12 then
           HR := 0;
        end;
    end;
  H := HR;
  DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
end;

procedure TDateTimeObj.SetMin(AValue: TMinute);
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  Mi := AValue;
  DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
end;

procedure TDateTimeObj.SetMonth(AValue: TMonth);
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  M := AValue;
  DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
end;

procedure TDateTimeObj.SetPM(AValue: Boolean);
begin
  if AMPM then
    begin
      DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
      if AValue then
        begin
          if H < 12 then
            H := H + 12;
        end
      else
        begin
          if H > 12 then
            H := H - 12;
        end;
      DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
    end;
end;

procedure TDateTimeObj.SetSecond(AValue: TSecond);
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  S := AValue;
  DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
end;

procedure TDateTimeObj.SetYear(AValue: TYear);
begin
  DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  Y := AValue;
  DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
end;

constructor TDateTimeObj.Create( IsAMPM : Boolean; aParent: TPersists);
begin
  fAMPM := IsAMPM;
  inherited Create(aParent);
  MakeNew;
end;

constructor TDateTimeObj.Copy(Src: TDateTimeObj);
begin
  Create( Src.fAMPM );
  DateTime := Src.DateTime;
end;

procedure TDateTimeObj.MakeNew;
begin
  DateTime := EncodeDateTime( 1, 1, 1, 0, 0, 0, 0);
end;

procedure TDateTimeObj.Save(TextIO: TTextIO);
begin
  SaveHeader( TextIO, Version );
  TextIO.WriteLN( DateTime );
  TextIO.WriteLn( fAMPM );
  UNMODIFY;
  SaveTrailer( TextIO );
end;

procedure TDateTimeObj.Read(TextIO: TTextIO; Version: Integer);
begin
  MakeNew;
  if Version >= 1 then
    begin
      TextIO.ReadLn( DateTime );
      TextIO.Readln( fAMPM );
    end;


end;

procedure TDateTimeObj.AddSeconds(Seconds: Double);
var
  Delta : Double;
begin
  Delta := Seconds / (24.0*(60.0 * 60.0));
  DateTime := DateTime + Delta;
  //DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  //S := S + Seconds;
  //DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
end;

procedure TDateTimeObj.AddMinutes(Minutes: Double);
var
  Delta : Double;
begin
  Delta := MInutes / (24.0*(60.0));
  DateTime := DateTime + Delta;
  //DecodeDateTime( DateTime, Y, M, D, H, Mi, S, U );
  //Mi := Mi + Minutes;
  //DateTime := EncodeDateTime( Y, M, D, H, Mi, S, U );
end;

initialization

  ObjectFactory.RegisterClass( TDateTimeObj.ClassType );

end.

