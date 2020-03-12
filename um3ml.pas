unit uM3ML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, uM3File, ustructures;

procedure ExportToM3ML(const Model: TM3File; const FileName: string);
procedure ImportFromM3ML(const Model: TM3File; const FileName: string);

var
  m3mlExport_SimpleValueCountLimit: UInt32 = 100;

implementation

uses
  uCommon;

procedure SetFieldValueBinary(const el: TDOMElement; const fData: Pointer; const fSize: Integer);
var
  p: Pointer;
  i: Integer;
  s: string;
begin
  el['binary'] := '1';
  s := #13#10;
  p := fData;
  i := 1;
  while i <= fSize do
  begin
    s := s + IntToHex(PByte(p)^,2);
    if (i mod 16) = 0 then s := s + #13#10
    else if (i mod 4) = 0 then s := s + ' ';
    inc(p);
    inc(i);
  end;
  if (fSize mod 16) <> 0 then
    s := s + #13#10;
  el.TextContent := s;
end;

procedure SetFieldValue(const el: TDOMElement; const F: TM3Field);
begin
  case F.fType of
    ftBinary: SetFieldValueBinary(el,F.fData,F.fSize);
    ftUInt8: el['value'] := IntToStr(pUInt8(F.fData)^);
    ftUInt16: el['value'] := IntToStr(pUInt16(F.fData)^);
    ftUInt32: el['value'] := IntToStr(pUInt32(F.fData)^);
    ftInt8: el['value'] := IntToStr(pInt8(F.fData)^);
    ftInt16: el['value'] := IntToStr(pInt16(F.fData)^);
    ftInt32: el['value'] := IntToStr(pInt32(F.fData)^);
    ftFloat: el['value'] := M3FloatToStr(PSingle(F.fData)^);
    ftRef: with Pm3ref(F.fData)^ do
      begin
        el['refIdx'] := IntToStr(refIndex);
        el['refCnt'] := IntToStr(refCount);
        el['refFlag']:= IntToHex(refFlags,8);
      end;
    ftRefSmall: with Pm3ref_small(F.fData)^ do
      begin
        el['refIdx'] := IntToStr(refIndex);
        el['refCnt'] := IntToStr(refCount);
      end;
  end;
end;

procedure ExportToM3ML(const Model: TM3File; const FileName: string);
var
  i, j, k: Integer;
  off: Integer;
  m3ml: TXMLDocument;
  root, m3tag, item, el: TDOMElement;
begin
  m3ml:= TXMLDocument.Create;
  try
    root := m3ml.CreateElement('m3root');
    root['structures'] := Structures.MD5String;
    m3ml.AppendChild(root);
    for i := 0 to Model.TagCount-1 do
    with Model[i]^ do
    begin
      m3tag := m3ml.CreateElement('m3tag');
      root.AppendChild(m3tag);
      m3tag['idx'] := IntToStr(Index);
      m3tag['name'] := StructName;
      m3tag['ver'] := IntToStr(Ver);
      for j := 0 to length(RefFrom)-1 do
      begin
        item := m3ml.CreateElement('refFrom');
        m3tag.AppendChild(item);
        item['from'] := RefFrom[j].rfName;
      end;
      if StructName = 'CHAR' then
      begin
        m3tag['value'] := PChar(Data);
      end
      else
      begin
        m3tag['itemSize'] := IntToStr(ItemSize);
        if (ItemSize = 1)and(ItemCount > 8) then
        begin
          item := m3ml.CreateElement('data');
          m3tag.AppendChild(item);
          SetFieldValueBinary(item,Data,ItemCount);
        end
        else if length(ItemFields) = 1 then
        begin
          m3tag['itemType'] := ItemFields[0].fTypeName;
          m3tag['simple'] := '1';
          for j := 0 to ItemCount-1 do
          begin
            item := m3ml.CreateElement('item');
            m3tag.AppendChild(item);
            ItemFields[0].fData := Data + (j*ItemSize) + ItemFields[0].fOffset;
            SetFieldValue(item,ItemFields[0]);
          end;
        end
        else
        begin
          for j := 0 to ItemCount-1 do
          begin
            item := m3ml.CreateElement('item');
            m3tag.AppendChild(item);
            for k := 0 to length(ItemFields)-1 do
            with ItemFields[k] do
              if fSize > 0 then
              begin
                fData := Data + (j*ItemSize) + fOffset;
                el := m3ml.CreateElement('field');
                item.AppendChild(el);
                el['name'] := fGroupName+fName;
                el['type'] := fTypeName;
                el['size'] := IntToStr(fSize);
                SetFieldValue(el,ItemFields[k]);
              end;
          end;
        end;
      end;
    end;

    WriteXMLFile(m3ml,FileName);
  finally
    m3ml.Free;
  end;
end;

procedure ImportFromM3ML(const Model: TM3File; const FileName: string);
var
  i: Integer;
  m3ml: TXMLDocument;
begin
  m3ml:= TXMLDocument.Create;
  try
    ReadXMLFile(m3ml,FileName);
    Model.InitEmptyModel(0);

    for i := 0 to Model.TagCount-1 do
    with Model[i]^ do
    begin

    end;
  finally
    m3ml.Free;
  end;
end;

end.

