unit uCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, ustructures;

const
  headerTag33 = $4D443333;
  headerTag34 = $4D443334;
  CHARTag     = $43484152;

var
  FloatDotFormat: TFormatSettings;

function FieldValToStr(const F: TM3Field): string;
function RepeatStr(const str: string; count: integer): string;
function GetTreeTagName(const tag: TM3Structure): string;

function GetChildDOMElement(const el: TDOMElement): TDOMElement;
procedure NextDOMElement(var el: TDOMElement);
function FindDOMElement(const el: TDOMElement; const ElementName: DOMString): TDOMElement;

function IsValidM3File(const FileName: string): boolean;

implementation

uses
  strutils;

function FieldValToStr(const F: TM3Field): string;
var
  i: integer;
  v: UInt32;
begin
  case F.fType of
    ftBinary: Result := Format('{Binary Data, size=%d}',[F.fSize]);
    ftUInt8: Result := Format('%u (0x%s)',[pUInt8(F.fData)^,IntToHex(pUInt8(F.fData)^,2)]);
    ftUInt16: Result := Format('%u (0x%s)',[pUInt16(F.fData)^,IntToHex(pUInt16(F.fData)^,4)]);
    ftUInt32: Result := Format('%u (0x%0:.8x)',[pUInt32(F.fData)^]);
    ftInt8: Result := Format('%d (0x%s)',[pInt8(F.fData)^,IntToHex(pUInt8(F.fData)^,2)]);
    ftInt16: Result := Format('%d (0x%s)',[pInt16(F.fData)^,IntToHex(pUInt16(F.fData)^,4)]);
    ftInt32: Result := Format('%d (0x%0:.8x)',[pInt32(F.fData)^]);
    ftFloat: Result := Format('%s (0x%.8x)',[FloatToStr(PSingle(F.fData)^,FloatDotFormat),pUInt32(F.fData)^]);
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
  if tag.StructName = 'CHAR' then
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

initialization
  FloatDotFormat.DecimalSeparator := '.';
  FloatDotFormat.ThousandSeparator := '.';
end.

