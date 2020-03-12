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
  umain, uCommon;

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
    else if (i mod 4) = 0 then s := s + ' | '
    else s := s + ' ';
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
    ftUInt8: el['value'] := IntToStr(pUInt8(F.fData)^);
    ftUInt16: el['value'] := IntToStr(pUInt16(F.fData)^);
    ftUInt32: el['value'] := IntToStr(pUInt32(F.fData)^);
    ftInt8: el['value'] := IntToStr(pInt8(F.fData)^);
    ftInt16: el['value'] := IntToStr(pInt16(F.fData)^);
    ftInt32: el['value'] := IntToStr(pInt32(F.fData)^);
    ftFloat: el['value'] := FloatToStr(PSingle(F.fData)^,FloatDotFormat);
    ftRef: with Pm3ref(F.fData)^ do
      begin
        el['refIdx'] := IntToStr(refIndex);
        el['refCnt'] := IntToStr(refCount);
        el['refFlag']:= '0x'+IntToHex(refFlags,8);
      end;
    ftRefSmall: with Pm3ref_small(F.fData)^ do
      begin
        el['refIdx'] := IntToStr(refIndex);
        el['refCnt'] := IntToStr(refCount);
      end;
    else SetFieldValueBinary(el,F.fData,F.fSize);
  end;
end;

procedure ExportToM3ML(const Model: TM3File; const FileName: string);
var
  i, j, k: Integer;
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
      m3tag['itemSize'] := IntToStr(ItemSize);
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
        if (ItemSize = 1)and(ItemCount > 8) then
        begin
          m3tag['binary'] := '1';
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

function GetBinaryDataSize(const BinText: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to length(BinText) do
    if BinText[i] in ['0'..'9','a'..'f','A'..'F'] then
      inc(Result);
  Result := Result div 2;
end;

procedure ReadBinaryData(const Data: Pointer; MaxCount: Integer; const BinText: string);
var
  p: Pointer;
  s: string;
  i: Integer;
begin
  if MaxCount < 1 then exit;
  s := '';
  p := Data;
  for i := 1 to length(BinText) do
    if BinText[i] in ['0'..'9','a'..'f','A'..'F'] then
    begin
      s := s + BinText[i];
      if length(s) = 2 then
      begin
        PByte(p)^ := StrToInt('0x'+s);
        inc(p);
        dec(MaxCount);
        s := '';
        if MaxCount = 0 then exit;
      end;
    end;
end;

function GuessTagValue(const Name: String): UInt32;
var
  l: integer;
  arr: packed array[0..3] of Byte;
begin
  Result := 0;
  l := length(Name);
  if (l > 0) and (l <= 4) then
  begin
    if l > 0 then arr[3] := Ord(Name[1]) else arr[3] := 0;
    if l > 1 then arr[2] := Ord(Name[2]) else arr[2] := 0;
    if l > 2 then arr[1] := Ord(Name[3]) else arr[1] := 0;
    if l > 3 then arr[0] := Ord(Name[4]) else arr[0] := 0;
    Move(arr[0],Result,4);
  end;
end;

procedure ReadM3TagItemField(const Data: Pointer; const Item: TDOMElement;
  const fType: TM3FieldTypes; const Size: Integer);
var
  i: Integer;
begin
  if item['binary'] = '1' then
  begin
    i := GetBinaryDataSize(item.TextContent);
    if i > Size then
    begin
      FMain.Log('M3ML Parser: binary data don''t fit in item size and will be truncated');
      i := Size;
    end;
    ReadBinaryData(Data,i,item.TextContent);
  end
  else
  begin
    case fType of
      ftUInt8: pUInt8(Data)^ := StrToInt(Item['value']);
      ftUInt16: pUInt16(Data)^ := StrToInt(Item['value']);
      ftUInt32: pUInt32(Data)^ := StrToInt(Item['value']);
      ftInt8: pInt8(Data)^ := StrToInt(Item['value']);
      ftInt16: pInt16(Data)^ := StrToInt(Item['value']);
      ftInt32: pInt32(Data)^ := StrToInt(Item['value']);
      ftFloat: PSingle(Data)^ := StrToFloat(Item['value'],FloatDotFormat);
      ftRef: with Pm3ref(Data)^ do
        begin
          refIndex := StrToInt(Item['refIdx']);
          refCount := StrToInt(Item['refCnt']);
          refFlags := StrToInt(Item['refFlag']);
        end;
      ftRefSmall: with Pm3ref(Data)^ do
        begin
          refIndex := StrToInt(Item['refIdx']);
          refCount := StrToInt(Item['refCnt']);
        end;
      else FMain.Log('M3ML Parser: Encountered invalid type of field or simple item');
    end;
  end;
end;

procedure ReadM3TagItem(var Struct: TM3Structure; const Item: TDOMElement;
  const Simple: Boolean; const SimpleType: TM3FieldTypes);
var
  i, sizeLeft: Integer;
  t: TM3FieldTypes;
  el: TDOMElement;
  p: Pointer;
begin
  i := Struct.ItemCount;
  ResizeStructure(Struct, i+1);
  p := Struct.Data + Struct.ItemSize * i;
  if Simple then
  begin // value is located in item element
    ReadM3TagItemField(p,item,SimpleType,Struct.ItemSize);
  end
  else
  begin // values are defined in child elements
    el := GetChildDOMElement(item);
    sizeLeft := Struct.ItemSize;
    while el <> nil do
    begin
      if el.TagName = 'field' then
      begin
        t := FieldTypeFromStr(el['type']);
        i := StrToIntDef(el['size'],FieldSizeFromType(t));
        if i > 0 then
        begin
          dec(sizeLeft,i);
          if sizeLeft < 0 then
          begin
            FMain.Log('M3ML Parser: Item "%d: %s": Fields data don''t fit in item size. Excess fields will be ignored',[Struct.Index, Struct.StructName]);
            Exit;
          end
          else
          begin
            ReadM3TagItemField(p,el,t,i);
            inc(p,i);
          end;
        end;
      end;
      NextDOMElement(el);
    end;
  end;
end;

procedure ReadM3Tag(var Struct: TM3Structure; const Idx: Integer; const m3tag: TDOMElement);
var
  L: Integer;
  s: string;
  t: TM3FieldTypes;
  item: TDOMElement;
  simple: Boolean;
begin
  with Struct do
  begin
    if Tag <> 0 then
      FMain.Log('M3ML Parser: Duplicate tag index %d',[Idx])
    else
    begin
      Index := Idx;
      StructName := m3tag['name'];
      if StructName = 'CHAR' then
      begin
        Tag := CHARTag;
        Ver := 0;
        s := m3tag['value'];
        ItemSize := 1;
        ItemCount := 0;
        ResizeStructure(Struct,length(s)+1);
        StrPCopy(Data,s);
        Exit;
      end;
      Ver := StrToIntDef(m3tag['ver'],0);
      if not Structures.GetTagInfoFromName(Struct) then
      begin
        Tag := GuessTagValue(StructName);
        if Tag = 0 then
        begin
          FMain.Log('M3ML Parser Error: Invalid tag name "%d: %s"',[Idx,StructName]);
          Exit;
        end;
      end;
      if ItemSize = 0 then
      begin
        ItemSize := StrToIntDef(m3tag['itemSize'],0);
        if ItemSize = 0 then
        begin
          FMain.Log('M3ML Parser Error: Tag "%d: %s" item size not defined',[Idx,StructName]);
          Tag := 0;
          Exit;
        end;
      end;
      if m3tag['binary'] = '1' then
      begin
        item := FindDOMElement(m3tag,'data');
        if item <> nil then
        begin
          s := item.TextContent;
          L := GetBinaryDataSize(s);
          ItemCount := L div ItemSize;
          if (L mod ItemSize) <> 0 then
          begin
            FMain.Log('M3ML Parser: Tag "%d: %s" binary data size is not divided by item size',[Idx,StructName]);
          end;
          if L > 0 then
          begin
            while (L mod 16) <> 0 do inc(L);
            ReAllocMem(Data, L);
            DataSize := L;
            FillChar(Data^,L,$AA);
            ReadBinaryData(Data,L,s);
          end;
        end;
      end
      else
      begin
        simple := (m3tag['simple'] = '1');
        t := FieldTypeFromStr(m3tag['itemType']);
        if simple and (ItemSize < FieldSizeFromType(t)) then
        begin
          FMain.Log('M3ML Parser Error: Tag "%d: %s" item size is less that size of simple item type',[Idx,StructName]);
          Tag := 0;
          Exit;
        end;
        item := GetChildDOMElement(m3tag);
        while item <> nil do
        begin
          if item.TagName = 'item' then
            ReadM3TagItem(Struct,item,simple,t);
          NextDOMElement(item);
        end;
      end;
    end;
  end;
end;

procedure ImportFromM3ML(const Model: TM3File; const FileName: string);
var
  i, j: Integer;
  max: Integer;
  m3ml: TXMLDocument;
  root, m3tag: TDOMElement;
begin
  m3ml:= TXMLDocument.Create;
  try
    ReadXMLFile(m3ml,FileName);
    root := m3ml.DocumentElement;
    if root['structures'] <> Structures.MD5String then
      FMain.Log('M3ML Parser: structures signature does not match');

    // first, lets count tags
    i := 0;
    max := 0;
    m3tag := GetChildDOMElement(root);
    while m3tag <> nil do
    begin
      if m3tag.TagName = 'm3tag' then
      begin
        j := StrToIntDef(m3tag['idx'],-1);
        if i <> j then
          FMain.Log('M3ML Parser: Index of m3tag (%d: "%s") don''t match tag position (%d)',[j,m3tag['name'],i]);
        if j > max then max := j;
        inc(i);
      end;
      NextDOMElement(m3tag);
    end;
    inc(max);
    if i<>max then
      FMain.Log('M3ML Parser: Number of tags (%d) don''t match (max index+1)=%d',[i,max]);
    if i > max then
      Model.InitEmptyModel(i)
    else
      Model.InitEmptyModel(max);

    // read data from tags with index
    // skip tags without index for now
    m3tag := GetChildDOMElement(root);
    while m3tag <> nil do
    begin
      i := StrToIntDef(m3tag['idx'],-1);
      if (m3tag.TagName = 'm3tag') and (i <> -1) then
        ReadM3Tag(Model[i]^,i,m3tag);
      NextDOMElement(m3tag);
    end;

    for i := 0 to Model.TagCount-1 do
    with Model[i]^ do
    begin

    end;
  finally
    m3ml.Free;
  end;
end;

end.

