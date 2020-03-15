unit uM3File;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ustructures;

type

  { TM3File }

  TM3File = class
  private
    FTags: array of TM3Structure;

    procedure ClearTags;
    function GetTag(Index: Integer): PM3Structure;
    function GetTagCount: Integer;
  public
    procedure LoadM3File(const FileName: string);
    procedure SaveM3File(const FileName: string);

    procedure InitEmptyModel(const NewTagCount: Integer);
    function AppendEmptyTag: Integer;
    procedure InsertEmptyTag(const Idx: Integer);
    procedure DeleteTag(const Idx: Integer);
    procedure DeleteTagCascade(const Idx: Integer);
    procedure ResetTag(const Idx: Integer);

    procedure MoveTagUp(const Idx: Integer);
    procedure MoveTagDown(const Idx: Integer);

    procedure ResetRefFrom;
    function RepairReferenceCount: Integer;
    function ScanReferences(const CHAROnly: Boolean): Integer;

    property Tags[Index: Integer]: PM3Structure read GetTag; default;
    property TagCount: Integer read GetTagCount;
  end;

implementation

uses
  umain, uCommon;

type
  m3TagListItem = packed record
    tag: UInt32;
    dataOffset: UInt32;
    dataCount: UInt32;
    ver: UInt32;
  end;

function StructNameFromTag(const Tag: UInt32):string;
var
  a: array[0..3] of AnsiChar;
begin
  move(Tag,a,4);
  Result := '';
  if a[3] <> #0 then
    Result := Result + a[3];
  if a[2] <> #0 then
    Result := Result + a[2];
  if a[1] <> #0 then
    Result := Result + a[1];
  if a[0] <> #0 then
    Result := Result + a[0];
end;

{ TM3File }

procedure TM3File.ClearTags;
var
  i: Integer;
begin
  for i := 0 to length(FTags)-1 do
  with FTags[i] do
  begin
    Freemem(Data);
    SetLength(ItemFields,0);
    SetLength(RefFrom,0);
  end;
  SetLength(FTags,0);
end;

function TM3File.GetTag(Index: Integer): PM3Structure;
begin
  if (Index >= 0) and (Index < length(FTags)) then
    Result := @FTags[Index]
  else
    Result := nil;
end;

function TM3File.GetTagCount: Integer;
begin
  Result := length(FTags);
end;

procedure TM3File.LoadM3File(const FileName: string);
var
  F: TFileStream;
  H: m3Header;
  tagList: array of m3TagListItem;
  i, tagDataSize: integer;
begin
  f := TFileStream.Create(FileName,fmShareDenyNone);
  try
    f.Read(H,sizeof(H));
    if (h.tag = headerTag33) or (h.tag = headerTag34) then
    begin
      ClearTags;
      SetLength(tagList,h.tagListCount);
      SetLength(FTags,h.tagListCount);
      f.Seek(H.tagListOffset,soFromBeginning);
      f.Read(tagList[0],sizeof(m3TagListItem)*h.tagListCount);
      for i := 0 to h.tagListCount-1 do
      begin
        if i = h.tagListCount-1 then
          tagDataSize := H.tagListOffset - tagList[i].dataOffset
        else
          tagDataSize := tagList[i+1].dataOffset - tagList[i].dataOffset;
        FTags[i].Tag := tagList[i].tag;
        FTags[i].Index := i;
        FTags[i].StructName := StructNameFromTag(FTags[i].Tag);
        FTags[i].Ver := tagList[i].ver;
        f.Seek(tagList[i].dataOffset,soFromBeginning);
        GetMem(FTags[i].Data, tagDataSize);
        FTags[i].DataSize := tagDataSize;
        f.Read(FTags[i].Data^, tagDataSize);
        FTags[i].ItemCount := tagList[i].dataCount;
        Structures.GetTagSize(FTags[i]);
      end;
    end
    else
      FMain.Log('M3 file header not found!');
  finally
    f.Free;
    SetLength(tagList,0);
  end;
end;

procedure TM3File.SaveM3File(const FileName: string);
var
  F: TFileStream;
  tagList: array of m3TagListItem;
  i, off: integer;
begin
  if (Pm3Header(FTags[0].Data)^.tag <> headerTag33) and (Pm3Header(FTags[0].Data)^.tag <> headerTag34) then
  begin
    FMain.Log('First tag is not M3 file header tag');
    Exit;
  end;
  // first calculate all offsets, fill tagList and set list offest in header
  SetLength(tagList, Length(FTags));
  off := 0;
  for i := 0 to length(FTags)-1 do
  begin
    tagList[i].tag := FTags[i].Tag;
    tagList[i].dataOffset := off;
    tagList[i].dataCount := FTags[i].ItemCount;
    tagList[i].ver := FTags[i].Ver;
    inc(off, FTags[i].DataSize);
  end;
  with Pm3Header(FTags[0].Data)^ do
  begin
    tagListOffset := off;
    tagListCount := length(FTags);
  end;

  // then write to file
  F := TFileStream.Create(FileName,fmCreate);
  try
    for i := 0 to length(FTags)-1 do
      F.Write(FTags[i].Data^,FTags[i].DataSize);
    f.Write(tagList[0],sizeof(m3TagListItem)*length(tagList));
  finally
    F.Free;
    SetLength(tagList,0);
  end;
end;

procedure TM3File.InitEmptyModel(const NewTagCount: Integer);
var
  i: integer;
begin
  ClearTags;
  SetLength(FTags,NewTagCount);
  for i := 0 to NewTagCount-1 do
  with FTags[i] do
  begin
    Tag := 0;
    Index := i;
    StructName := '';
    Description := '';
    Ver := 0;
    Getmem(Data,16);
    DataSize := 16;
    ItemSize := 0;
    ItemCount := 0;
  end;
end;

function TM3File.AppendEmptyTag: Integer;
begin
  Result := TagCount;
  SetLength(FTags,Result+1);
  with FTags[Result] do
  begin
    Tag := 0;
    Index := Result;
    StructName := '';
    Description := '';
    Ver := 0;
    Getmem(Data,16);
    DataSize := 16;
    ItemSize := 0;
    ItemCount := 0;
  end;
end;

procedure TM3File.InsertEmptyTag(const Idx: Integer);
var
  i, j: Integer;
  p: Pm3ref_small;
begin
  i := TagCount;
  SetLength(FTags,i+1);

  while i > Idx do
  begin
    ResetRefFrom;
    for j := 0 to length(FTags[i-1].RefFrom)-1 do
    with FTags[i-1].RefFrom[j] do
    begin
      p := FTags[rfTagIndex].Data + rfRefFieldOffset;
      Inc(p^.refIndex);
    end;
    FTags[i] := FTags[i-1];
    FTags[i].Index := i;
    Dec(i);
  end;

  with FTags[i] do
  begin
    Tag := 0;
    Index := i;
    StructName := '';
    Description := '';
    Ver := 0;
    Getmem(Data,16);
    DataSize := 16;
    ItemSize := 0;
    ItemCount := 0;
  end;
end;

procedure TM3File.DeleteTag(const Idx: Integer);
var
  i, j: Integer;
  p: Pm3ref_small;
begin
  if (Idx < 0) or (Idx >= TagCount) Then Exit;
  i := Idx;

  ResetRefFrom;
  for j := 0 to length(FTags[i].RefFrom)-1 do
  with FTags[i].RefFrom[j] do
  begin
    p := FTags[rfTagIndex].Data + rfRefFieldOffset;
    p^.refIndex := 0;
    p^.refCount := 0;
  end;

  with FTags[i] do
  begin
    Freemem(Data,DataSize);
    SetLength(ItemFields,0);
    SetLength(RefFrom,0);
  end;

  inc(i);
  while i < TagCount do
  begin
    ResetRefFrom;
    for j := 0 to length(FTags[i].RefFrom)-1 do
    with FTags[i].RefFrom[j] do
    begin
      p := FTags[rfTagIndex].Data + rfRefFieldOffset;
      dec(p^.refIndex);
    end;
    FTags[i-1] := FTags[i];
    FTags[i-1].Index := i-1;
    inc(i);
  end;

  SetLength(FTags,TagCount-1);
  ResetRefFrom;
end;

procedure TM3File.DeleteTagCascade(const Idx: Integer);
var
  arr: array of Integer;
  checkIdx: Integer;
  i, j, arrIdx: Integer;
  add: boolean;
begin
  if (Idx < 0) or (Idx >= TagCount) Then Exit;
  // need to find indexes of tags to delete
  ResetRefFrom;
  SetLength(arr,1);
  arr[0] := Idx;
  arrIdx := 0;
  while arrIdx < length(arr) do
  begin
    checkIdx := arr[arrIdx];
    for i := 0 to TagCount-1 do
      if length(FTags[i].RefFrom) > 0 then
      begin
        add := true;
        for j := 0 to length(FTags[i].RefFrom)-1 do
          if FTags[i].RefFrom[j].rfTagIndex <> checkIdx then
          begin
            add := false;
            Break;
          end;
        if add then
        begin
          for j := 0 to Length(arr)-1 do
            if arr[j] = i then
            begin
              add := false;
              Break;
            end;
          if add then
          begin
            j := length(arr);
            SetLength(arr,j+1);
            arr[j] := i;
          end;
        end;
      end;
    inc(arrIdx);
  end;

  // sort indexes, bigger first
  for i := 0 to length(arr)-2 do
  begin
    checkIdx := i;
    for j := i+1 to length(arr)-1 do
      if arr[j] > arr[checkIdx] then
        checkIdx := j;
    if i<>checkIdx then
    begin
      arrIdx := arr[i];
      arr[i] := arr[checkIdx];
      arr[checkIdx] := arrIdx;
    end;
  end;

  // delete tags
  for i := 0 to length(arr)-1 do
    DeleteTag(arr[i]);
end;

procedure TM3File.ResetTag(const Idx: Integer);
begin
  if (Idx >= 0) and (Idx < TagCount) Then
  with FTags[Idx] do
  begin
    ResizeStructure(FTags[Idx],0);
    SetLength(ItemFields,0);
    SetLength(RefFrom,0);
    Tag := 0;
    Index := Idx;
    StructName := '';
    Description := '';
    Ver := 0;
    ItemSize := 0;
    ItemCount := 0;
  end;
end;

procedure TM3File.MoveTagUp(const Idx: Integer);
var
  tmp: TM3Structure;
  i: Integer;
  p: Pm3ref_small;
begin
  if (Idx > 0) and (Idx < TagCount) then
  begin
    ResetRefFrom;
    for i := 0 to length(FTags[Idx].RefFrom)-1 do
    with FTags[Idx].RefFrom[i] do
    begin
      p := FTags[rfTagIndex].Data + rfRefFieldOffset;
      dec(p^.refIndex);
    end;
    for i := 0 to length(FTags[Idx-1].RefFrom)-1 do
    with FTags[Idx-1].RefFrom[i] do
    begin
      p := FTags[rfTagIndex].Data + rfRefFieldOffset;
      inc(p^.refIndex);
    end;
    tmp := FTags[Idx];
    FTags[Idx] := FTags[Idx-1];
    FTags[Idx-1] := tmp;
    FTags[Idx].Index := Idx;
    FTags[Idx-1].Index := Idx-1;
    ResetRefFrom;
  end;
end;

procedure TM3File.MoveTagDown(const Idx: Integer);
begin
  MoveTagUp(Idx+1);
end;

procedure TM3File.ResetRefFrom;
var
  i, j, k, idx: Integer;
  pRef: Pm3ref_small;
begin
  // this function have 5 (five) indexes and 3 (three) nested loops
  // stay strong and don't get lost
  for i := 0 to TagCount - 1 do
    SetLength(FTags[i].RefFrom,0);
  for i := 0 to TagCount - 1 do
  begin
    Structures.GetStructureInfo(FTags[i]);
    with FTags[i] do
    if ItemSize >= sizeof(m3ref_small) then
    begin
      for j := 0 to length(ItemFields)-1 do
      begin
        if (ItemFields[j].fType in [ftRef,ftRefSmall]) then
          for idx := 0 to ItemCount-1 do
          begin
            pRef := Data + (ItemSize*idx) + ItemFields[j].fOffset;
            with pRef^ do
            if (refCount > 0) and (refIndex > 0) and (refIndex < TagCount) then
            begin
              k := length(FTags[refIndex].RefFrom);
              SetLength(FTags[refIndex].RefFrom, k+1);
              FTags[refIndex].RefFrom[k].rfTagIndex := i;
              FTags[refIndex].RefFrom[k].rfItemIndex := idx;
              FTags[refIndex].RefFrom[k].frFieldRow := j + 1;
              FTags[refIndex].RefFrom[k].rfRefFieldOffset := (ItemSize*idx) + ItemFields[j].fOffset;
              FTags[refIndex].RefFrom[k].rfName := Format(
                '%d: %s [%d] -> %s (refCount = %d)',
                [i, FTags[i].StructName, idx, ItemFields[j].fGroupName+ItemFields[j].fName, refCount]
              );
            end;
          end;
      end;
    end;
  end;
end;

function TM3File.RepairReferenceCount: Integer;
var
  i, j: Integer;
  p: Pm3ref_small;
begin
  ResetRefFrom;
  Result := 0;
  for i := 0 to TagCount-1 do
    for j := 0 to length(FTags[i].RefFrom)-1 do
    with FTags[i].RefFrom[j] do
    begin
      p := FTags[rfTagIndex].Data + rfRefFieldOffset;
      if p^.refCount <> FTags[i].ItemCount then
      begin
        p^.refCount := FTags[i].ItemCount;
        inc(Result);
      end;
    end;
end;

function TM3File.ScanReferences(const CHAROnly: Boolean): Integer;
var
  i, j, idx: Integer;
  pRef: Pm3ref;
begin
  Result := 0;
  for i := 0 to TagCount - 1 do
  begin
    Structures.GetStructureInfo(FTags[i]);
    with FTags[i] do
    if ItemSize >= sizeof(m3ref_small) then
    begin
      for j := 0 to length(ItemFields)-1 do
      begin
        if (ItemFields[j].fType in [ftRef,ftRefSmall]) then
          for idx := 0 to ItemCount-1 do
          begin
            pRef := Data + (ItemSize*idx) + ItemFields[j].fOffset;
            with pRef^ do
            begin
              if (refCount > 0) and (refIndex > 0) and (refIndex < TagCount) then
              begin
                if (FTags[refIndex].ItemCount <> refCount) and ((FTags[refIndex].Tag = CHARTag) or not CHAROnly) then
                begin
                  refCount := FTags[refIndex].ItemCount;
                  inc(Result);
                end;
              end
              else
              begin
                if (refCount <> 0) or (refIndex <> 0) then
                  inc(Result);
                refCount := 0;
                refIndex := 0;
                if ItemFields[j].fType = ftRef then
                  refFlags := 0;
              end;
            end;
          end;
      end;
    end;
  end;
end;

end.

