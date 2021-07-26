unit UAnimListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, uM3File, ustructures, uCommon;

type
  PAnimSequenceData = ^TAnimSequenceData;
  TAnimSequenceData = packed record
    frames: m3ref;
    flags: UInt32;
    fend: UInt32;
    keys: m3ref;
  end;

  PAnimSTC_Info = ^TAnimSTC_Info;
  TAnimSTC_Info = record
    itemIndex: Integer;
    name: string;
    animIds: array of UInt32;
    sequenceData: array of PAnimSequenceData;
    //sequenceDataTypes: array of Integer;
  end;

  PAnimID_Info = ^TAnimID_Info;
  TAnimID_Info = record
    animId: UInt32;
    tagPath: string;
    fieldName: string;
    parentTag: PM3Structure;
    parentItemIndex: Integer;
    usedBySTC: array of PAnimSTC_Info;
  end;

  TAnimDuplicateAction = (daReturnExising, daMakeNew, daReturnNil);

  { TFAnimListView }

  TFAnimListView = class(TForm)
    comboFilterBySTC: TComboBox;
    comboSTC: TComboBox;
    Label1: TLabel;
    lbAnimIDs: TListBox;
    PageControl: TPageControl;
    pnlAnimIDBottom: TPanel;
    pnlAnimIdTop: TPanel;
    sbFilterIDs: TSpeedButton;
    sbSortByPath: TSpeedButton;
    Splitter2: TSplitter;
    TabAnimIDs: TTabSheet;
    TabAnimList: TTabSheet;
    treeIDs: TTreeView;
    procedure comboFilterBySTCChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure lbAnimIDsDblClick(Sender: TObject);
    procedure sbFilterIDsClick(Sender: TObject);
    procedure sbSortByPathClick(Sender: TObject);
  private
    FM3: TM3File;

    FSTCollections: PM3Structure;

    FAnimIds: array of TAnimID_Info;
    FAnimSTCs: array of TAnimSTC_Info;
    function GetAnimId(const aID: UInt32): PAnimID_Info;
    function NewAnimId(const aID: UInt32; const DuplicateAction: TAnimDuplicateAction = daMakeNew): PAnimID_Info;
    procedure ClearAnimIds;
    procedure ParseAnimData;

    function GetTreeTagItemName(aTag: TM3Structure; const aItemIdx: Integer): string;
    function GetTagItemFullPath(aTag: TM3Structure; const aItemIdx: Integer): string;

    function FilterPassAnimID(const animId: PAnimID_Info): boolean;

    procedure ResetIDTree;
    procedure ResetSTCList;
    function GetIDTreeTagNode(const aTag: PM3Structure; const aIdx: Integer): TTreeNode;
  public
    procedure ShowEditor(const M3File: TM3File);
    procedure ResetAnimView;
  end;

var
  FAnimListView: TFAnimListView;

implementation

uses
  umain;

{
const
  SDTYPE_EV_EVNT = 0;
  SDTYPE_2V_VEC2 = 1;
  SDTYPE_3V_VEC3 = 2;
  SDTYPE_4Q_QUAT = 3;
  SDTYPE_CC_COL  = 4;
  SDTYPE_R3_REAL = 5;
  SDTYPE_UNK00   = 6;
  SDTYPE_S6_I16_ = 7;
  SDTYPE_U6_U16_ = 8;
  SDTYPE_UNK01   = 9;
  SDTYPE_U3_U32_ = 10;
  SDTYPE_FG_FLAG = 11;
  SDTYPE_MB_BNDS = 12;
}
const
  SDTYPE_MAX = 12;

{$R *.lfm}

{ TFAnimListView }

procedure TFAnimListView.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFAnimListView.comboFilterBySTCChange(Sender: TObject);
begin
  if sbFilterIDs.Down then
    ResetIDTree;
end;

procedure TFAnimListView.FormDestroy(Sender: TObject);
begin
  FMain.FreeAnimListForm;
end;

procedure TFAnimListView.lbAnimIDsDblClick(Sender: TObject);
var
  node: TObject;
begin
  node := lbAnimIDs.Items.Objects[lbAnimIDs.ItemIndex];
  if Assigned(node) and (node is TTreeNode) then
    treeIDs.Select(TTreeNode(node));
end;

procedure TFAnimListView.sbFilterIDsClick(Sender: TObject);
begin
  ResetIDTree;
end;

procedure TFAnimListView.sbSortByPathClick(Sender: TObject);
begin
  ResetIDTree;
end;

function TFAnimListView.GetAnimId(const aID: UInt32): PAnimID_Info;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to length(FAnimIds)-1 do
    if FAnimIds[i].animId = aID then
    begin
      Result := @FAnimIds[i];
      Exit;
    end;
end;

function TFAnimListView.NewAnimId(const aID: UInt32;
  const DuplicateAction: TAnimDuplicateAction): PAnimID_Info;
var
  i: Integer;
begin
  Result := GetAnimId(aID);
  if Assigned(Result) then
  begin
    FMain.Log('Warning! Duplicating animation ID found: %.8x (%d)',[aID, aID]);
    case DuplicateAction of
      daReturnExising: Exit;
      //daMakeNew: ;
      daReturnNil: Exit(nil);
    end;
  end;
  i := length(FAnimIds);
  SetLength(FAnimIds,i+1);
  FAnimIds[i].animId := aID;
  Result := @FAnimIds[i];
end;

procedure TFAnimListView.ClearAnimIds;
var
  i: Integer;
begin
  for i := 0 to length(FAnimIds)-1 do
  begin
    SetLength(FAnimIds[i].usedBySTC,0);
  end;
  SetLength(FAnimIds,0);
  for i := 0 to length(FAnimSTCs)-1 do
  begin
    SetLength(FAnimSTCs[i].animIds,0);
    SetLength(FAnimSTCs[i].sequenceData,0);
  end;
  SetLength(FAnimSTCs,0);
end;

procedure TFAnimListView.ParseAnimData;
var
  i, j, k: Integer;
  Pid: PUInt32;
  refType, refIdx, refBaseOffset: UInt32;
  idInfo: PAnimID_Info;
  pTmp, pSD: PM3Structure;
begin
  ClearAnimIds;
  if FM3.TagCount = 0 then Exit;
  // collect anim ids
  for i := 0 to fm3.TagCount-1 do
  if Structures.GetStructureInfo(fm3[i]^,false) then
    with fm3[i]^ do
    begin
      for j := 0 to length(ItemFields)-1 do
        if ItemFields[j].fName = 'animId' then
        begin
          for k := 0 to ItemCount -1 do
          begin
            Pid := Data + ItemSize * k + ItemFields[j].fOffset;
            idInfo := NewAnimId(pid^);
            with idInfo^ do
            begin
              parentTag := FM3[i];
              parentItemIndex := k;
              fieldName := ItemFields[j].fGroupName+ItemFields[j].fName;
              tagPath := GetTagItemFullPath(parentTag^,k);
            end;
          end;
        end;
    end;
  // parse STC_ data
  FSTCollections := FM3.FollowRefField(FM3.GetModelTag^,0,'sequenceTransformationCollections');
  if Assigned(FSTCollections) then
  with FSTCollections^ do
  begin
    SetLength(FAnimSTCs,ItemCount);
    for i := 0 to ItemCount-1 do
    begin
      FAnimSTCs[i].itemIndex := i;
      pTmp := fm3.FollowRefField(FSTCollections^,i,'name');
      if Assigned(pTmp) and (pTmp^.SpecialType <> sstCharBinary) then
        FAnimSTCs[i].name := PChar(pTmp^.Data)
      else
        FAnimSTCs[i].name := '';

      pTmp := fm3.FollowRefField(FSTCollections^,i,'animIds');
      if Assigned(pTmp) and (pTmp^.ItemCount > 0) then
      begin
        SetLength(FAnimSTCs[i].animIds,pTmp^.ItemCount);
        for j := 0 to pTmp^.ItemCount-1 do
        begin
          FAnimSTCs[i].animIds[j] := pUInt32(pTmp^.Data+j*pTmp^.ItemSize)^;
          idInfo := GetAnimId(FAnimSTCs[i].animIds[j]);
          // it's possible that anim ID does not reference any field for SDEV-EVNT animation data
          if Assigned(idInfo) then
          begin
            k := Length(idInfo^.usedBySTC);
            SetLength(idInfo^.usedBySTC,k+1);
            idInfo^.usedBySTC[k] := @FAnimSTCs[i];
          end;
        end;
        pTmp := fm3.FollowRefField(FSTCollections^,i,'animRefs');
        if Assigned(pTmp) and (pTmp^.ItemCount > 0) then
        begin
          SetLength(FAnimSTCs[i].sequenceData,pTmp^.ItemCount);
          if length(FAnimSTCs[i].animIds) <> pTmp^.ItemCount then
          begin
            FMain.Log('Warning: %d:%s[%d] "%s" animation ID count (%d) and SD references count (%d) don''t match',
              [Index,StructName,i,FAnimSTCs[i].name,length(FAnimSTCs[i].animIds),pTmp^.ItemCount]);
            if length(FAnimSTCs[i].animIds) > pTmp^.ItemCount then
            begin
              SetLength(FAnimSTCs[i].sequenceData,length(FAnimSTCs[i].animIds));
              for k := pTmp^.ItemCount to length(FAnimSTCs[i].animIds)-1 do
                FAnimSTCs[i].sequenceData[k] := nil;
            end;
          end;
          // find SDEV field offset, it will be used for decoding references
          refBaseOffset := 0;
          if Structures.GetStructureInfo(FSTCollections^) then
          begin
            for j := 0 to length(FSTCollections^.ItemFields)-1 do
              if FSTCollections^.ItemFields[j].fName = 'sdev' then
              begin
                refBaseOffset := FSTCollections^.ItemFields[j].fOffset;
                Break;
              end;
          end;
          if refBaseOffset = 0 then
            refBaseOffset := 48; // magic number manually calculated for STC_ version 4
          for j := 0 to pTmp^.ItemCount-1 do
          begin
            refIdx := pUInt32(pTmp^.Data+j*pTmp^.ItemSize)^;
            refType := (refIdx shr 16) and $FFFF;
            refIdx := refIdx and $FFFF;
            if refType > SDTYPE_MAX then
            begin
              FMain.Log('Warning: %d:%s[%d] "%s" SD reference type at index %d is out of bounds (type %d > %d)',
                [Index,StructName,i,FAnimSTCs[i].name,j,refType,SDTYPE_MAX]);
              FAnimSTCs[i].sequenceData[j] := nil;
              Continue;
            end;
            pSD := fm3[Pm3ref(Data+ItemSize*i+refBaseOffset+sizeof(m3ref)*refType)^.refIndex];
            if Assigned(pSD) then
            begin
              if refIdx >= pSD^.ItemCount then
              begin
                FMain.Log('Warning: %d:%s[%d] "%s" SD reference at index %d referencing tag %d:%s item out of bounds (idx %d > %d)',
                  [Index,StructName,i,FAnimSTCs[i].name,j,pSD^.Index,pSD^.StructName,refIdx,pSD^.ItemCount-1]);
                FAnimSTCs[i].sequenceData[j] := nil;
              end
              else
                FAnimSTCs[i].sequenceData[j] := pSD^.Data + pSD^.ItemSize*refIdx;
            end
            else
            begin
              FMain.Log('Warning: %d:%s[%d] "%s" SD reference type at index %d is not referencing any tag (type %d)',
                [Index,StructName,i,FAnimSTCs[i].name,j,refType]);
              FAnimSTCs[i].sequenceData[j] := nil;
            end;
            // check anim ID is in the list
            if Assigned(FAnimSTCs[i].sequenceData[j]) and (j < length(FAnimSTCs[i].animIds)) then
            begin
              idInfo := GetAnimId(FAnimSTCs[i].animIds[j]);
              if not Assigned(idInfo) then
              begin
                pSD := FM3[FAnimSTCs[i].sequenceData[j]^.keys.refIndex];
                // check if animation data is SDEV-EVNT
                if not Assigned(pSD) or (pSD^.StructName <> 'EVNT') then
                begin
                  FMain.Log('Warning: %d:%s[%d] "%s" anim ID 0x%.8x (%d) not found',
                    [Index,StructName,i,FAnimSTCs[i].name,FAnimSTCs[i].animIds[j],FAnimSTCs[i].animIds[j]]);
                end;
              end;
            end;
          end;
        end
        else
        begin
          SetLength(FAnimSTCs[i].sequenceData,0);
          FMain.Log('Warning: %d:%s[%d] "%s" has 0 animation SD references',[Index,StructName,i,FAnimSTCs[i].name]);
        end;
      end
      else
      begin
        SetLength(FAnimSTCs[i].animIds,0);
        FMain.Log('Warning: %d:%s[%d] "%s" has 0 animation IDs',[Index,StructName,i,FAnimSTCs[i].name]);
      end;
    end;
  end;
end;

function TFAnimListView.GetTreeTagItemName(aTag: TM3Structure;
  const aItemIdx: Integer): string;
var
  s: string;
  i: Integer;
  ref: pm3ref;
begin
  if (aTag.StructName = 'CHAR') and (aTag.SpecialType <> sstCharBinary) then
  begin
    Result := PChar(aTag.Data);
  end
  else
  begin
    Structures.GetStructureInfo(aTag, false);
    s := '';
    for i := 0 to length(aTag.ItemFields)-1 do
    with aTag.ItemFields[i] do
      if fName = 'name' then
      begin
        ref := aTag.Data + aTag.ItemSize * aItemIdx + fOffset;
        s := format(' name="%s"',[GetTreeTagItemName(fm3[ref^.refIndex]^,0)]);
        Break;
      end;
    if (aTag.ItemCount=1) then
      result := Format('%d:%s%s',[aTag.Index,aTag.StructName,s])
    else
      result := Format('%d:%s[%d]%s',[aTag.Index,aTag.StructName,aItemIdx,s]);
  end;
end;

function TFAnimListView.GetTagItemFullPath(aTag: TM3Structure;
  const aItemIdx: Integer): string;
var
  pref: PM3Structure;
  ref_from: TM3RefFrom;
begin
  with aTag do
  begin
    Result := GetTreeTagItemName(aTag,aItemIdx);
    if (Index > 0) and (length(RefFrom) > 0) then
    begin
      ref_from := RefFrom[0];
      pref := FM3[ref_from.rfTagIndex];
    end
    else
      pref := nil;
  end;
  while Assigned(pref) and (pref^.Index > 0) do
    with pref^ do
    begin
      Result := Format('%s - %s -> ',[
        GetTreeTagItemName(pref^,ref_from.rfItemIndex),
        ref_from.rfFieldName
      ])+Result;

      if (length(RefFrom) > 0) then
      begin
        ref_from := RefFrom[0];
        pref := FM3[ref_from.rfTagIndex];
      end
      else
        pref := nil;
    end;
end;

function TFAnimListView.FilterPassAnimID(const animId: PAnimID_Info): boolean;
var
  i: integer;
begin
  Result := true;
  if sbFilterIDs.Down then
  begin
    Result := false;
    if Assigned(animId) then
    with animId^ do
    begin
      if comboFilterBySTC.ItemIndex = 0 then
        Result := (Length(usedBySTC) > 0)
      else
      begin
        i := 0;
        while (not Result) and (i < Length(usedBySTC)) do
        begin
          Result := (usedBySTC[i]^.itemIndex = (comboFilterBySTC.ItemIndex-1));
          inc(i);
        end;
      end;
    end;
  end;
end;

procedure TFAnimListView.ResetIDTree;
var
  i, j: Integer;
  tagNode: TTreeNode;
  sl: TStringList;
begin
  treeIDs.Items.Clear;
  lbAnimIDs.Clear;
  sl := TStringList.Create;
  try
    for i := 0 to length(FAnimIds)-1 do
    with FAnimIds[i] do
    begin
      if FilterPassAnimID(@FAnimIds[i]) then
      begin
        tagNode := GetIDTreeTagNode(parentTag,parentItemIndex);
        tagNode := treeIDs.Items.AddChild(tagNode,format('%s = 0x%.8x (%d)',[fieldName,animId,animId]));
        if sbSortByPath.Down then
          sl.AddObject(
            '%s -> %s: 0x%.8x (%d)',
            [tagPath,fieldName,animId,animId],
            tagNode
          )
        else
          sl.AddObject(
            '0x%.8x (%d): %s -> %s',
            [animId,animId,tagPath,fieldName],
            tagNode
          );
        for j := 0 to length(usedBySTC)-1 do
        with usedBySTC[j]^ do
          treeIDs.Items.AddChild(tagNode,format('used by %d:%s',[itemIndex,name]));
        tagNode.Collapse(true);
      end;
    end;
    sl.Sort;
    lbAnimIDs.Items.Assign(sl);
  finally
    sl.Free;
  end;
end;

procedure TFAnimListView.ResetSTCList;
var
  i: Integer;
begin
  comboSTC.Clear;
  comboFilterBySTC.Clear;
  comboFilterBySTC.Items.Add('All');
  comboSTC.Items.NameValueSeparator := ':';
  comboFilterBySTC.Items.NameValueSeparator := ':';
  for i := 0 to length(FAnimSTCs)-1 do
  with FAnimSTCs[i] do
  begin
    comboSTC.Items.Add('%d:%s',[i,name]);
    comboFilterBySTC.Items.Add('%d:%s',[i,name]);
  end;
  comboFilterBySTC.ItemIndex := 0;
end;

function TFAnimListView.GetIDTreeTagNode(const aTag: PM3Structure;
  const aIdx: Integer): TTreeNode;
var
  node: TTreeNode;
begin
  node := nil;
  with treeIDs.Items.GetEnumerator do
  try
    while MoveNext do
    if Assigned(Current.Data) and (Current.Data = aTag) then
    begin
      if Current.ImageIndex = aIdx then
      begin
        Result := Current;
        Exit;
      end;
      if Current.ImageIndex = -1 then
        node := Current;
    end;
  finally
    Free;
  end;
  if not Assigned(node) then // tag root node does not exists
  begin
    if length(aTag^.RefFrom) > 0 then
    begin
      with aTag^.RefFrom[0] do
      if rfTagIndex > 0 then
        node := GetIDTreeTagNode(fm3[rfTagIndex],rfItemIndex);
    end;
    if Assigned(node) then
      node := treeIDs.Items.AddChildObject(node,GetTreeTagName(aTag^),aTag)
    else // parent node not found, create tag root at tree root
      node := treeIDs.Items.AddObject(nil,GetTreeTagName(aTag^),aTag);
  end;
  Result := treeIDs.Items.AddChildObject(node,GetTreeTagItemName(aTag^,aIdx),aTag);
  Result.ImageIndex := aIdx;
end;

procedure TFAnimListView.ShowEditor(const M3File: TM3File);
begin
  FM3 := M3File;
  ResetAnimView;
  Show;
end;

procedure TFAnimListView.ResetAnimView;
begin
  ParseAnimData;
  ResetSTCList;
  ResetIDTree;
end;

end.

