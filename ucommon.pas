(*
    This file is a part of "M3 Editor" project <https://github.com/tangorcraft/m3editor/>.
    Copyright (C) 2020  Ivan Markov (TangorCraft)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)
unit uCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Laz2_DOM, ustructures;

const
  headerTag33 = $4D443333;
  headerTag34 = $4D443334;
  CHARTag     = $43484152;

var
  FloatDotFormat: TFormatSettings;

function FieldValToStr(const F: TM3Field): string;
function RepeatStr(const str: string; count: integer): string;
function GetTreeTagName(const tag: TM3Structure): string;

function ReadFieldData(const tag: TM3Structure; const FieldName: string;
  const ItemIndex: Integer; var Val; const ValSize: Integer): boolean;
function WriteFieldData(const tag: TM3Structure; const FieldName: string;
  const ItemIndex: Integer; var Val; const ValSize: Integer): boolean;

function GetChildDOMElement(const el: TDOMElement): TDOMElement;
procedure NextDOMElement(var el: TDOMElement);
function FindDOMElement(const el: TDOMElement; const ElementName: DOMString): TDOMElement;

function IsValidM3File(const FileName: string): boolean;

function M3ClampColor(const C: Single): Single;
function VEC3ToColor(const vec: m3VEC3_color): TColor;

function FloatToStrM3(const F: single): string;

function GuessTagValue(Name: String): UInt32;

implementation

uses
  strutils;

function FieldValToStr(const F: TM3Field): string;
var
  i: integer;
  v: UInt32;
begin
  Result := '';
  case F.fType of
    ftBinary: Result := Format('{Binary Data, size=%d}',[F.fSize]);
    ftUInt8: Result := Format('%u (0x%s)',[pUInt8(F.fData)^,IntToHex(pUInt8(F.fData)^,2)]);
    ftUInt16: Result := Format('%u (0x%s)',[pUInt16(F.fData)^,IntToHex(pUInt16(F.fData)^,4)]);
    ftUInt32: Result := Format('%u (0x%0:.8x)',[pUInt32(F.fData)^]);
    ftInt8: Result := Format('%d (0x%s)',[pInt8(F.fData)^,IntToHex(pUInt8(F.fData)^,2)]);
    ftInt16: Result := Format('%d (0x%s)',[pInt16(F.fData)^,IntToHex(pUInt16(F.fData)^,4)]);
    ftInt32: Result := Format('%d (0x%0:.8x)',[pInt32(F.fData)^]);
    ftFloat: Result := Format('%s (0x%.8x)',[FloatToStrM3(PSingle(F.fData)^),pUInt32(F.fData)^]);
    ftRef: with Pm3ref(F.fData)^ do
      Result := Format('refIdx: %d (Cnt: %d) (Flags = 0x%.8x)',[refIndex,refCount,refFlags]);
    ftRefSmall: with Pm3ref_small(F.fData)^ do
      Result := Format('refIdx: %d (Cnt: %d)',[refIndex,refCount]);
    ftSubStruct: Result := '{Sub Structure: "' + F.fTypeName + '"}';
  end;
  if F.fTypeFlag then
  begin
    v := 1;
    for i := 0 to 31 do
    begin
      if (F.fTypeFlagBits[i] <> '') and ((pUInt32(F.fData)^ and v) <> 0) then
        Result := Result + ' ' + F.fTypeFlagBits[i];
      v := v shl 1;
    end;
  end;
end;

function RepeatStr(const str: string; count: integer): string;
begin
  Result := '';
  while count > 0 do
  begin
    dec(count);
    Result := Result + str;
  end;
end;

function GetTreeTagName(const tag: TM3Structure): string;
var
  s: string;
begin
  if (tag.StructName = 'CHAR') and (tag.SpecialType <> sstCharBinary) then
  begin
    s := PChar(tag.Data);
    Result:=Format('%d: %s (%d) "%s"',[tag.Index,tag.StructName,length(s)+1,s]);
  end
  else
  begin
    if tag.Ver = 0 then
      s := ''
    else
      s := 'V'+IntToStr(tag.Ver)+', ';
    result := Format('%d: %s (%sCnt %d)',[tag.Index,tag.StructName,s,tag.ItemCount]);
  end;
end;

function ReadFieldData(const tag: TM3Structure; const FieldName: string;
  const ItemIndex: Integer; var Val; const ValSize: Integer): boolean;
var
  i, size: Integer;
  p: Pointer;
begin
  Result := false;
  if (ItemIndex < 0) or (ItemIndex >= tag.ItemCount) then Exit;
  i := 0;
  while (i < length(tag.ItemFields)) and (tag.ItemFields[i].fName <> FieldName) do inc(i);
  if i >= length(tag.ItemFields) then Exit;
  if tag.ItemFields[i].fSize > ValSize then Exit;
  if tag.ItemFields[i].fSize < ValSize then
  begin
    FillChar(Val,ValSize,0);
    size := tag.ItemFields[i].fSize;
  end
  else
    size := ValSize;
  p := tag.Data + tag.ItemSize * ItemIndex + tag.ItemFields[i].fOffset;
  Move(p^,Val,size);
  Result := true;
end;

function WriteFieldData(const tag: TM3Structure; const FieldName: string;
  const ItemIndex: Integer; var Val; const ValSize: Integer): boolean;
var
  i, size: Integer;
  p: Pointer;
begin
  Result := false;
  if (ItemIndex < 0) or (ItemIndex >= tag.ItemCount) then Exit;
  i := 0;
  while (i < length(tag.ItemFields)) and (tag.ItemFields[i].fName <> FieldName) do inc(i);
  if i >= length(tag.ItemFields) then Exit;
  if tag.ItemFields[i].fSize < ValSize then Exit;
  p := tag.Data + tag.ItemSize * ItemIndex + tag.ItemFields[i].fOffset;
  if tag.ItemFields[i].fSize > ValSize then
  begin
    FillChar(p^,tag.ItemFields[i].fSize,0);
    size := ValSize;
  end
  else
    size := tag.ItemFields[i].fSize;
  Move(Val,p^,size);
  Result := true;
end;

function GetChildDOMElement(const el: TDOMElement): TDOMElement;
var
  n: TDOMNode;
begin
  n := el.FirstChild;
  while (n <> nil) and not (n is TDOMElement) do
    n := n.NextSibling;
  if n = nil then
    Result := nil
  else
    Result := n as TDOMElement;
end;

procedure NextDOMElement(var el: TDOMElement);
var
  n: TDOMNode;
begin
  n := el.NextSibling;
  while (n <> nil) and not (n is TDOMElement) do
    n := n.NextSibling;
  if n = nil then
    el := nil
  else
    el := n as TDOMElement;
end;

function FindDOMElement(const el: TDOMElement; const ElementName: DOMString): TDOMElement;
var
  n: TDOMNode;
begin
  n := el.FindNode(ElementName);
  if (n = nil) or not (n is TDOMElement) then
    Result := nil
  else
    Result := n as TDOMElement;
end;

function IsValidM3File(const FileName: string): boolean;
var
  tag: UInt32;
begin
  Result := false;
  with TFileStream.Create(FileName,fmShareDenyNone) do
  try
    if Read(tag,4) <> 4 then Exit;
    if (tag = headerTag33) or (tag = headerTag34) then Result := true;
  finally
    Free;
  end;
end;

function M3ClampColor(const C: Single): Single;
begin
  if C > 1 then Result := 1
  else if C < 0 then Result := 0
  else Result := C;
end;

function VEC3ToColor(const vec: m3VEC3_color): TColor;
var
  col: m3Color;
  i: integer;
begin
  i := round(vec.R*255);
  if i >= 255 then
    col.red := 255
  else if i <= 0 then
    col.red := 0
  else col.red := i;

  i := round(vec.G*255);
  if i >= 255 then
    col.green := 255
  else if i <= 0 then
    col.green := 0
  else col.green := i;

  i := round(vec.B*255);
  if i >= 255 then
    col.blue := 255
  else if i <= 0 then
    col.blue := 0
  else col.blue := i;

  Result := RGBToColor(col.red,col.green,col.blue);
end;

function FloatToStrM3(const F: single): string;
Var
  P, PE, Q, Exponent: Integer;
begin
    Str(F:21, Result);
    { Delete leading spaces }
    while Result[1] = ' ' do
      System.Delete(Result, 1, 1);
    P := Pos('.', Result);
    if P=0 then
      Exit; { NAN or other special case }
    { Consider removing exponent }
    PE:=Pos('E',Result);
    if PE > 0 then begin
      { Read exponent }
      Q := PE+2;
      Exponent := 0;
      while (Q <= Length(Result)) do begin
        Exponent := Exponent*10 + Ord(Result[Q])-Ord('0');
        Inc(Q);
      end;
      if Result[PE+1] = '-' then
        Exponent := -Exponent;
      if (P+Exponent < PE) and (Exponent > -6) then begin
        { OK to remove exponent }
        SetLength(Result,PE-1); { Trim exponent }
        if Exponent >= 0 then begin
          { Shift point to right }
          for Q := 0 to Exponent-1 do begin
            Result[P] := Result[P+1];
            Inc(P);
          end;
          Result[P] := '.';
          P := 1;
          if Result[P] = '-' then
            Inc(P);
          while (Result[P] = '0') and (P < Length(Result)) and (Result[P+1] <> '.') do
            { Trim leading zeros; conversion above should not give any, but occasionally does
              because of rounding }
            System.Delete(Result,P,1);
        end else begin
          { Add zeros at start }
          Insert(Copy('00000',1,-Exponent),Result,P-1);
          Result[P-Exponent] := Result[P-Exponent-1]; { Copy leading digit }
          Result[P] := '.';
          if Exponent <> -1 then
            Result[P-Exponent-1] := '0';
        end;
        { Remove trailing zeros, but leave .0 at the end }
        Q := Length(Result);
        while (Q > 0) and (Result[Q] = '0') and (Result[Q-1] <> '.') do
          Dec(Q);
        if (Q = 0) or ((Q=1) and (Result[1] = '-')) then
          Result := '0'
        else
          SetLength(Result,Q);
      end else begin
        { Need exponent, but remove superfluous characters }
        { Delete trailing zeros }
        while Result[PE-1] = '0' do begin
          System.Delete(Result,PE-1,1);
          Dec(PE);
        end;
        { If number ends in decimal point, remove it }
        if Result[PE-1] = '.' then begin
          System.Delete(Result,PE-1,1);
          Dec(PE);
        end;
        { delete superfluous + in exponent }
        if Result[PE+1]='+' then
          System.Delete(Result,PE+1,1)
        else
          Inc(PE);
        while Result[PE+1] = '0' do
          { Delete leading zeros in exponent }
          System.Delete(Result,PE+1,1)
      end;
    end;
end;

function GuessTagValue(Name: String): UInt32;
var
  i, l: integer;
  arr: packed array[0..3] of Byte;
begin
  Result := 0;
  l := length(Name);
  Move(Result,arr[0],4);
  for i := 0 to 3 do
  begin
    if l = 0 then Break;
    arr[i] := ord(Name[l]);
    dec(l);
  end;
  Move(arr[0],Result,4);
end;

initialization
  FloatDotFormat.DecimalSeparator := '.';
  FloatDotFormat.ThousandSeparator := '.';
end.

