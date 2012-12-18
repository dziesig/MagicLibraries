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

unit datetimeframe1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ExtCtrls,

  datetimeobj1, common1, stringsubs;

type

  { TFrame1 }

  TFrame1 = class(TFrame)
    AMPMCB: TComboBox;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    CenturyCB: TComboBox;
    DayCB: TComboBox;
    HourCB: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MinuteCB: TComboBox;
    MonthCB: TComboBox;
    YearCB: TComboBox;
    procedure CenturyCBChange(Sender: TObject); // Actually any ComboBoxes change
    procedure FrameClick(Sender: TObject);
  private
    fDateTimeObj: TDateTimeObj;
    fMaxCentury: Word;
    fMinCentury: Word;

    fOnChange : TNotifyEvent;
    procedure SetDateTimeObj(AValue: TDateTimeObj);
    procedure SetMaxCentury(AValue: Word);
    procedure SetMinCentury(AValue: Word);
    { private declarations }
  protected
    procedure PopulateCenturyCB;
    procedure PopulateDaysCB( Year : TYear; Month : TMonth );
    procedure PopulateHourCB;
    procedure UpdateDateTimeObj;
  public
    { public declarations }
    constructor Create( TheOwner : TComponent ); override;

    procedure Update;

    property DateTimeObj : TDateTimeObj read fDateTimeObj write SetDateTimeObj;

    property MinCentury : Word read fMinCentury write SetMinCentury default 1800;
    property MaxCentury : Word read fMaxCentury write SetMaxCentury;

    property OnChange   : TNotifyEvent read fOnChange write fOnChange;
  end;

implementation

{$R *.lfm}

{ TFrame1 }

uses
  StrUtils;


procedure TFrame1.FrameClick(Sender: TObject);
begin
  if Assigned( fOnChange ) then fOnChange( fDateTimeObj );
end;

procedure TFrame1.CenturyCBChange(Sender: TObject);
begin
  UpdateDateTimeObj;
end;

procedure TFrame1.SetMaxCentury(AValue: Word);
begin
  if fMaxCentury=AValue then Exit;
  fMaxCentury:=AValue;
  if fMinCentury > fMaxCentury then
    fMinCentury := fMaxCentury;
  PopulateCenturyCB;
end;

procedure TFrame1.SetDateTimeObj(AValue: TDateTimeObj);
begin
  fDateTimeObj:=AValue;
  if fDateTimeObj.AMPM then
    begin
      AMPMCB.Visible := True;
      Width := AMPMCB.Left + AMPMCB.Width + CenturyCB.Left;
    end
  else
    begin
      AMPMCB.Visible := False;
      Width := MinuteCB.Left + MinuteCB.Width + CenturyCB.Left;
    end;
  PopulateHourCB;
  Update;
end;

procedure TFrame1.SetMinCentury(AValue: Word);
begin
  if fMinCentury=AValue then Exit;
  fMinCentury:=AValue;
  if fMaxCentury < fMinCentury then
    fMaxCentury := fMinCentury;
  PopulateCenturyCB;
end;

procedure TFrame1.PopulateCenturyCB;
var
  I : Integer;
begin
  CenturyCB.Clear;
  for I := fMinCentury div 100 to fMaxCentury div 100 do
    CenturyCB.Items.Add( IntToStr(I) );
  CenturyCB.ItemIndex := 0;
end;

procedure TFrame1.PopulateDaysCB(Year: TYear; Month: TMonth);
const
  DaysInMonth : array [1..12] of Word = ( 31,29,31,30,31,30,31,31,30,31,30,31 );
var
  I, D, LastDay : Word;
begin
  D := DayCB.ItemIndex + 1;
  if Month = 2 then
    if IsLeapYear( Year ) then
      LastDay := 29
    else
      LastDay := 28
  else
    LastDay := DaysInMonth[ Month ];

  DayCB.Clear;
  for I := 1 to LastDay do
    DayCB.Items.Add( IntToStr( I ) );
  D := Min( D, LastDay );
  DayCB.ItemIndex := D - 1;
end;

procedure TFrame1.PopulateHourCB;
var
  TwelveHour : Boolean;
  I          : Word;
begin
  TwelveHour := False;
  if Assigned( fDateTimeObj ) then
    TwelveHour := fDateTimeObj.AMPM;
  HourCB.Clear;
  if TwelveHour then
    begin
      HourCB.Items.Add( '12' );
      for I := 1 to 11 do
        HourCB.Items.Add( Dec2Numb( I, 2, 10 ));
    end
  else
    for I := 0 to 23 do
      HourCB.Items.Add( Dec2Numb( I, 2, 10 ));
  HourCB.ItemIndex := 0;
end;

procedure TFrame1.UpdateDateTimeObj;
const
  DaysInMonth : array [1..12] of Word = ( 31,29,31,30,31,30,31,31,30,31,30,31 );
var
  Y, M, D, H  : Word;
begin
  Y := fMinCentury + CenturyCB.ItemIndex*100;
  Y := Y + YearCB.ItemIndex;
  M := MonthCB.ItemIndex + 1;
  PopulateDaysCB( Y, M );
  if not Assigned( fDateTimeObj ) then exit;  // Don't bother if no DTO there.
  fDateTimeObj.Year := Y;
  fDateTimeObj.Month := M;
  fDateTimeObj.Day := DayCB.ItemIndex + 1;
  H := StrToInt( HourCB.Items[HourCB.ItemIndex] );
  fDateTimeObj.Hour := H;
  fDatetimeObj.Min := MinuteCB.ItemIndex;
  if fDateTimeObj.AMPM then
    fDateTimeObj.PM := AMPMCB.ItemIndex > 0;
  if Assigned( fOnChange ) then fOnChange( fDateTimeObj );
end;

constructor TFrame1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  MinCentury := 0100;
  MaxCentury := 3000;
  fOnChange  := nil;
end;

procedure TFrame1.Update;
var
  Century : Word;
  Year    : Word;
  I       : Word;
begin
  Century := (fDateTimeObj.Year div 100) * 100;
  Year    := fDateTimeObj.Year mod 100;
  if Century < MinCentury then MinCentury := Century;
  if Century > MaxCentury then MaxCentury := Century;
  I := (Century - MinCentury) div 100;
  CenturyCB.ItemIndex := I;
  YearCB.ItemIndex := Year;
  MonthCB.ItemIndex := fDateTimeObj.Month - 1;
  PopulateDaysCB( DateTimeObj.Year,DateTimeObj.Month );
  PopulateHourCB;
  DayCB.ItemIndex := fDateTimeObj.Day - 1;
  if fDateTimeObj.AMPM then
    begin
      for I := 0 to 11 do
        if StrToInt( HourCB.Items[I] ) = fDateTimeObj.Hour then
          HourCB.ItemIndex := I;
    end
  else
    HourCB.ItemIndex := fDateTimeObj.Hour;
  MinuteCB.ItemIndex := fDateTimeObj.Min;
end;

end.

