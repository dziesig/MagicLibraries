unit StringSubs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{------------------------------------------------------------------------------}
{ Interprets string (as output by floatToStrF( ... ffCurrency ...) and returns }
{ a float.  Raises exception if invalid format.  ( empty string = 0.00 )       }
{------------------------------------------------------------------------------}
function DollarsToFloat( Value : String ) : Extended;

{------------------------------------------------------------------------------}
{ Returns a Dollar string with the $ at a constant position (Width).           }
{------------------------------------------------------------------------------}
function FloatToDollars( Value : Extended; Width : Integer = 10 ) : String;

function FloatToPercent( Value : Extended; Width : Integer = 8 ) : String;

{------------------------------------------------------------------------------}
{ Empty strings can be non-zero length but contain only tabs and spaces.       }
{------------------------------------------------------------------------------}

function Empty( S : String ) : Boolean;

{------------------------------------------------------------------------------}
{ Indents result (String or File Line) by Count * BaseIndent spaces            }
{ Does NOT take into account pre-existing characters (a la Tabs)               }
{------------------------------------------------------------------------------}
procedure IndentBy( var F : TextFile; Count : Integer; BaseIndent : Integer = 4 );
function  IndentBy( const Text : String; Count : Integer; BaseIndent : Integer = 4 ) : String; overload;

{------------------------------------------------------------------------------}
{ Interprets string as percent (ignores %) (e.g. 90% -> 0.90, empty = 0.0)     }
{------------------------------------------------------------------------------}
function PercentToFloat( Value : String ) : Extended;

{------------------------------------------------------------------------------}
{ Same as StrToFloat and StrToInt except Empty String returns 0                }
{------------------------------------------------------------------------------}
function StringToFloat( Value : String ) : Extended;
function StringToInt( Value : String ) : Integer;

function BoolToStr( Value : Boolean ) : String;

{------------------------------------------------------------------------------}
{ Function type definitions for UnitTest driver program                        }
{------------------------------------------------------------------------------}

type
  TExtendedFunctionTest = function ( Value : String ) : Extended of object;
  TStringFunctionTest = function ( Value : Extended; Width : Integer ) : String of object;
  TBooleanFunctionTest = function ( Value : String ) : Boolean of object;
  TStringIntFunctionTest = function (Value : String) : Integer of object;
  TIndentByString = function ( Value : String; Count : Integer; BaseIndent : Integer ) : String of object;
  TIndentByFile = function( Count, BaseIndent : Integer ) : String of object;
implementation

function DollarsToFloat(Value: String): Extended;
var
  I : Integer;
  S : String;
begin
  S := '';
  for I := 1 to Length( Value ) do
    begin
      if Value[I] in ['0'..'9','.','+','-'] then
        S := S + Value[I]
      else
        if not (Value[I] in [',','$']) then
          raise Exception.Create( 'Value is not a valid USD value' );
    end;
  Result := Trunc(StringToFloat(S) * 100) / 100.0;
end;

function Empty(S: String): Boolean;
begin
  Result := Trim(S) = '';
end;

function FloatToDollars(Value: Extended; Width: Integer): String;
var
  Temp : String;
  Dec  : Integer;
  Wid  : Integer;
  I, J : Integer;
begin
  Temp := FloatToStrF( Value, ffFixed,Width,2);
  Dec  := Pos('.',Temp);
  J := Dec;
  Result := Copy(Temp,Dec,3);
  for I := 1 to pred(Dec) do
    begin
      Result := Temp[Dec - I] + Result;
      if ((I mod 3) = 0) and (I < pred(Dec)) then
        Result := ',' + Result;
    end;
  Wid := Width - Length(Result);
  if Wid > 0 then
    for I := 1 to pred(Wid) do
      Result := ' ' + Result;
  Result := '$' + Result;
end;

function FloatToPercent(Value: Extended; Width: Integer): String;
var
  Temp : String;
  Dec  : Integer;
  I, J : Integer;
  Wid  : Integer;
begin
  Temp := FloatToStrF( Value * 100.0, ffFixed,Width,2);
  Dec  := Pos('.',Temp);
  J := Dec;
  Result := Copy(Temp,Dec,3);
  for I := 1 to pred(Dec) do
    begin
      Result := Temp[Dec - I] + Result;
      if ((I mod 3) = 0) and (I < pred(Dec)) then
        Result := ',' + Result;
    end;
  Wid := Width - Length(Result);
  if Wid > 0 then
    for I := 1 to pred(Wid) do
      Result := ' ' + Result;
  Result := Result + '%';
end;

procedure IndentBy( var F : TextFile; Count : Integer; BaseIndent : Integer = 4 );
var
  I: Integer;
begin
  for I := 1 to Count * BaseIndent do
    Write(F,' ');
end;

function IndentBy(const Text: String; Count: Integer; BaseIndent: Integer = 4 ): String;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Count * BaseIndent do
    Result := Result + ' ';
  Result := Result + Text;
end;

function PercentToFloat(Value: String): Extended;
var
  I : Integer;
  S : String;
begin
  S := '';
  for I := 1 to Length( Value ) do
    begin
      if Value[I] in ['0'..'9','.','+','-'] then
        S := S + Value[I]
      else
        if not (Value[I] in [',','%']) then
          raise Exception.Create( 'Value is not a valid Percentage value' );
    end;
  Result := Trunc(StringToFloat(S) * 100) / 10000.0;
end;

function StringToFloat(Value: String): Extended;
var
  I : Integer;
  S : String;
begin
  Result := 0.0;
  S := '';
  for I := 1 to Length( Value ) do
    begin
      if Value[I] in ['0'..'9','.','+','-'] then
        S := S + Value[I]
      else
        if not (Value[I] in [',']) then
          raise Exception.Create( 'Value is not a valid Float value' );
    end;
  if not Empty( S ) then Result := StrToFloat( S );
end;

function StringToInt(Value: String): Integer;
var
  I : Integer;
  S : String;
begin
  if Empty(Value) then
    Result := 0
  else
    begin
      S := '';
      for I := 1 to Length( Value ) do
        begin
          if Value[I] in ['0'..'9','+','-'] then
            S := S + Value[I]
          else
            if not (Value[I] in [',']) then
              raise Exception.Create( 'Value is not a valid Integer value' );
        end;

      Result := StrToInt( S );
    end;
end;

function BoolToStr(Value: Boolean): String;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

end.
