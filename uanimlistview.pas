unit UAnimListView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, Grids, uM3File, ustructures, uCommon, Types;

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

  TAnimGridRowData = record
    animID: UInt32;
    animInfo: PAnimID_Info;
    animRowTitle: string;
    keyTag: PM3Structure;
    frames: array of Int32;
  end;


  { TFAnimListView }

  TFAnimListView = class(TForm)
    comboFilterBySTC: TComboBox;
    comboSTC: TComboBox;
    dgAnimationKeyTable: TDrawGrid;
    edtKGSearch: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblFullPath: TLabel;
    lbAnimIDs: TListBox;
    PageControl: TPageControl;
    PanelBottom: TPanel;
    pnlAnimIDBottom: TPanel;
    pnlAnimIdTop: TPanel;
    sbFilterIDs: TSpeedButton;
    sbSortByPath: TSpeedButton;
    sbHideEmptyFrames: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    sgKeyTagTable: TStringGrid;
    TabAnimIDs: TTabSheet;
    TabAnimList: TTabSheet;
    treeIDs: TTreeView;
    procedure comboFilterBySTCChange(Sender: TObject);
    procedure comboSTCSelect(Sender: TObject);
    procedure dgAnimationKeyTableDblClick(Sender: TObject);
    procedure dgAnimationKeyTableDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure dgAnimationKeyTableMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure dgAnimationKeyTableSelection(Sender: TObject; aCol,
      aRow: Integer);
    procedure edtKGSearchKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure lbAnimIDsDblClick(Sender: TObject);
    procedure sbFilterIDsClick(Sender: TObject);
    procedure sbHideEmptyFramesClick(Sender: TObject);
    procedure sbSortByPathClick(Sender: TObject);
    procedure sgKeyTagTableDblClick(Sender: TObject);
  private
    FM3: TM3File;

    FSTCollections: PM3Structure;

    FAnimIds: array of TAnimID_Info;
    FAnimSTCs: array of TAnimSTC_Info;
    function GetAnimId(const aID: UInt32): PAnimID_Info;
    function NewAnimId(const aID: UInt32; const DuplicateAction: TAnimDuplicateAction = daMakeNew): PAnimID_Info;
    procedure ClearAnimIds;
    procedure ParseAnimData;

    function GetTagItemName(aTag: TM3Structure; const aItemIdx: Integer; const multiLine: Boolean = false): string;
    function GetTagItemFullPath(aTag: TM3Structure; const aItemIdx: Integer): string;

    function FilterPassAnimID(const animId: PAnimID_Info): boolean;

    procedure ResetIDTree;
    procedure ResetSTCList;
    function GetIDTreeTagNode(const aTag: PM3Structure; const aIdx: Integer): TTreeNode;

    procedure EditCHARbyRef(const F: TM3Field);
    procedure EditInt8Field(const F: TM3Field);
    procedure EditInt16Field(const F: TM3Field);
    procedure EditInt32Field(const F: TM3Field);
    procedure EditFloatField(const F: TM3Field);
    procedure EditFlagField(const F: TM3Field);
    procedure EditRefField(const F: TM3Field);
    procedure EditVEC3asColor(const F: TM3Field);
    procedure EditCOLasColor(const F: TM3Field);
  private
    // anim-keys grid
    FkgMouseCol: Integer;
    FkgCurSTCIdx: Integer;
    FkgRows: array of TAnimGridRowData;
    FkgSelRow: Integer;
    FkgSelFrameIdx: Integer;
    FkgSelFrame: Int32;
    FkgColToFrame: array of Int32;
    FkgFrameLast: Integer;

    FkgKeyTagItemData: Pointer;

    FkgSearch: string;
    function kgRowFilterPass(rTitle: string): boolean;

    function kgFrameToCol(const aFrame:Int32): Integer;
    function kgFrameAdd(const aFrame:Int32): Integer;

    procedure ClearKGData;
    procedure DisplayKGData(const STC_idx: Integer);
    procedure SelectFrame(const aRow, aFrameIdx: Integer);
    procedure UpdateKeyTagTable;
  public
    procedure ShowEditor(const M3File: TM3File);
    procedure ResetAnimView;
  end;

var
  FAnimListView: TFAnimListView;

implementation

uses
  umain, uEditByte, uEditWord, uEditInteger, uEditFlags, uEditFloat, uEditString, uColorEditor, uRefEdit;

const
  COL_Name = 0;
  COL_Type = 1;
  COL_Info = 2;
  COL_Value = 3;

const
  SDTYPE_MAX = 12;

{$R *.lfm}

function ExtraxtBaseFieldName(const animIdFieldName: string): string;
var
  i: Integer;
begin
  i := length(animIdFieldName);
  // from string like 'location.header.animId' we need to extract 'location'
  while (i > 0) and (animIdFieldName[i] <> '.') do
    dec(i);
  dec(i);
  while (i > 0) and (animIdFieldName[i] <> '.') do
    dec(i);
  Result := Copy(animIdFieldName,1,i-1);
end;

procedure DrawDiamond(const Canvas: TCanvas; const Rect: TRect);
var
  pts: array[0..3] of TPoint;
  mx, my: Integer;
begin
  mx := (Rect.Left+Rect.Right) div 2;
  my := (Rect.Top+Rect.Bottom) div 2;

  pts[0].x := mx;
  pts[0].y := my-16;

  pts[1].x := mx+16;
  pts[1].y := my;

  pts[2].x := mx;
  pts[2].y := my+16;

  pts[3].x := mx-16;
  pts[3].y := my;
  Canvas.Polygon(@pts[0],4);
end;

procedure DrawTopArrow(const Canvas: TCanvas; const Rect: TRect);
var
  pts: array[0..2] of TPoint;
  mx, my: Integer;
begin
  mx := (Rect.Left+Rect.Right) div 2;
  my := Rect.Top+4;

  pts[0].x := mx-6;
  pts[0].y := my;

  pts[1].x := mx+6;
  pts[1].y := my;

  pts[2].x := mx;
  pts[2].y := my+8;

  Canvas.Polygon(@pts[0],3);
end;

procedure DrawLeftArrow(const Canvas: TCanvas; const Rect: TRect);
var
  pts: array[0..2] of TPoint;
  mx, my: Integer;
begin
  mx := Rect.Left+2;
  my := (Rect.Top+Rect.Bottom) div 2;

  pts[0].x := mx;
  pts[0].y := my-6;

  pts[1].x := mx;
  pts[1].y := my+6;

  pts[2].x := mx+8;
  pts[2].y := my;

  Canvas.Polygon(@pts[0],3);
end;

function ExtractWord(var S: string; const C: Char = ' '): string;
var
  i: integer;
begin
  i := 1;
  while (i <= length(s)) and (s[i]<>c) do
    inc(i);
  Result:=Copy(S,1,i-1);
  Delete(s,1,i);
end;

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

procedure TFAnimListView.comboSTCSelect(Sender: TObject);
begin
  if comboSTC.ItemIndex <> FkgCurSTCIdx then
  begin
    FkgCurSTCIdx := comboSTC.ItemIndex;
    DisplayKGData(comboSTC.ItemIndex);
    SelectFrame(0,0);
  end;
end;

procedure TFAnimListView.dgAnimationKeyTableDblClick(Sender: TObject);
var
  i, c, r: Integer;
begin
  r := dgAnimationKeyTable.Row-1;
  c := dgAnimationKeyTable.Col-1;
  if (r >= 0) and (r < length(FkgRows)) then
  with FkgRows[r] do
  begin
    if sbHideEmptyFrames.Down then
      c := FkgColToFrame[c];
    for i := 0 to length(frames)-1 do
      if frames[i] = c then
      begin
        SelectFrame(r,i);
        Exit;
      end;
  end;
end;

procedure TFAnimListView.dgAnimationKeyTableDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  styl: TTextStyle;
  i: integer;
begin
  with dgAnimationKeyTable.Canvas do
  begin
    styl := TextStyle;
    if (aCol = 0) and (aRow = 0) then
    begin
      Line(aRect.TopLeft,aRect.BottomRight);
      aRect.Inflate(-8,-8);
      styl.Alignment := taLeftJustify;
      styl.Layout := tlBottom;
      TextRect(aRect,aRect.Left,0,'Animation',styl);
      styl.Alignment := taRightJustify;
      styl.Layout := tlTop;
      TextRect(aRect,0,aRect.Top,'Frame',styl);
      Exit;
    end;
    if (aCol = FkgMouseCol) then
    begin
      Pen.Color := clRed;
      i := (aRect.Left+aRect.Right) div 2;
      if aRow = 0 then
        Line(i,aRect.Bottom-8,i,aRect.Bottom)
      else
        Line(i,aRect.Top,i,aRect.Bottom);
    end;
    if (aCol = dgAnimationKeyTable.Col) or (aRow = dgAnimationKeyTable.Row) then
    begin
      Pen.Width := 2;
      Pen.Color := clBlack;
    end;
    if aRow = 0 then
    begin
      if sbHideEmptyFrames.Down then
        i := FkgColToFrame[aCol-1]
      else
        i := aCol-1;
      if i = FkgSelFrame then
      begin
        Brush.Color := clLime;
        Pen.Width := 1;
        DrawTopArrow(dgAnimationKeyTable.Canvas,aRect);
      end;
      styl.Alignment := taCenter;
      styl.Layout := tlCenter;
      TextRect(aRect,0,0,IntToStr(i),styl);
      Exit;
    end;
    if comboSTC.ItemIndex = -1 then Exit;
    with FkgRows[aRow-1] do
    begin
      if aCol = 0 then
      begin
        if aRow-1 = FkgSelRow then
        begin
          Brush.Color := clLime;
          Pen.Width := 1;
          DrawLeftArrow(dgAnimationKeyTable.Canvas,aRect);
        end;
        styl.Alignment := taCenter;
        styl.Layout := tlCenter;
        styl.SingleLine := false;
        TextRect(aRect,0,0,animRowTitle,styl);
        Exit;
      end;
      i := (aRect.Top+aRect.Bottom) div 2;
      Line(aRect.Left+4,i,aRect.Right-4,i);
      if sbHideEmptyFrames.Down then
        aCol := FkgColToFrame[aCol-1]
      else
        Dec(aCol);
      if (FkgSelRow = aRow-1) and (FkgSelFrame = aCol) then
      begin
        Brush.Color := clHighlight;
        Rectangle(aRect);
      end;
      for i := 0 to length(frames)-1 do
        if frames[i] = aCol then
        begin
          if Assigned(keyTag) then
          begin
            if i < keyTag^.ItemCount then
              Brush.Color := clYellow
            else
              Brush.Color := clGray;
          end
          else
            Brush.Color := clRed;
          DrawDiamond(dgAnimationKeyTable.Canvas,aRect);
          Exit;
        end;
    end;
  end;
end;

procedure TFAnimListView.dgAnimationKeyTableMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  c: integer;
begin
  c := dgAnimationKeyTable.MouseToCell(Point(x,y)).x;
  if c < 1 then c := 1;
  if (c <> FkgMouseCol) then
  begin
    FkgMouseCol := c;
    dgAnimationKeyTable.Invalidate;
  end;
end;

procedure TFAnimListView.dgAnimationKeyTableSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  dgAnimationKeyTable.Invalidate;
end;

procedure TFAnimListView.edtKGSearchKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    FkgSearch := AnsiLowerCase(Trim(edtKGSearch.Text));
    DisplayKGData(FkgCurSTCIdx);
    SelectFrame(0,0);
  end;
end;

procedure TFAnimListView.FormDestroy(Sender: TObject);
begin
  ClearAnimIds;
  ClearKGData;
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

procedure TFAnimListView.sbHideEmptyFramesClick(Sender: TObject);
begin
  if sbHideEmptyFrames.Down then
    dgAnimationKeyTable.ColCount := length(FkgColToFrame)+1
  else
    dgAnimationKeyTable.ColCount := FkgFrameLast+2;
end;

procedure TFAnimListView.sbSortByPathClick(Sender: TObject);
begin
  ResetIDTree;
end;

procedure TFAnimListView.sgKeyTagTableDblClick(Sender: TObject);
var
  idx: integer;
  FM3Struct: PM3Structure;
begin
  idx := sgKeyTagTable.Row - 1;
  FM3Struct := FkgRows[FkgSelRow].keyTag;
  if (idx >= 0) and (idx < length(FM3Struct^.ItemFields)) then
  with FkgRows[FkgSelRow].keyTag^.ItemFields[idx] do
  case fType of
    ftBinary: ShowMessage('Editor for binary values is not implemented.');
    ftUInt8,
    ftInt8:
      begin
        if fTypeFlag then
          EditFlagField(FM3Struct^.ItemFields[idx])
        else
          EditInt8Field(FM3Struct^.ItemFields[idx]);
      end;
    ftUInt16,
    ftInt16:
      begin
        if fTypeFlag then
          EditFlagField(FM3Struct^.ItemFields[idx])
        else
          EditInt16Field(FM3Struct^.ItemFields[idx]);
      end;
    ftUInt32,
    ftInt32:
      begin
        if fTypeFlag then
          EditFlagField(FM3Struct^.ItemFields[idx])
        else
          EditInt32Field(FM3Struct^.ItemFields[idx]);
      end;
    ftFloat: EditFloatField(FM3Struct^.ItemFields[idx]);
    ftRef, ftRefSmall:
      begin
        if Copy(fRefTo,1,4) = 'CHAR' then
          EditCHARbyRef(FM3Struct^.ItemFields[idx])
        else
          EditRefField(FM3Struct^.ItemFields[idx]);
      end
    else
      begin
        if Copy(fTypeName,1,4) = 'VEC3' then
          EditVEC3asColor(FM3Struct^.ItemFields[idx])
        else if Copy(fTypeName,1,3) = 'COL' then
          EditCOLasColor(FM3Struct^.ItemFields[idx])
        else
          ShowMessageFmt('Editor for "%s" structure in not implemented.',[fTypeName]);
      end;
  end;
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
  refType, refIdx: UInt32;
  idInfo: PAnimID_Info;
  pTmp, pSD: PM3Structure;
  refBaseOffset: Pointer;
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
              SetLength(FAnimSTCs[i].sequenceData,length(FAnimSTCs[i].animIds));
          end;
          for k := 0 to length(FAnimSTCs[i].sequenceData)-1 do
            FAnimSTCs[i].sequenceData[k] := nil;
          // find SDEV field offset, it will be used for decoding references
          refBaseOffset := nil;
          if Structures.GetStructureInfo(FSTCollections^) then
            refBaseOffset := GetFieldPointer(FSTCollections^,'sdev',i);
          if Assigned(refBaseOffset) then
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
            pSD := fm3[Pm3ref(refBaseOffset+sizeof(m3ref)*refType)^.refIndex];
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

function TFAnimListView.GetTagItemName(aTag: TM3Structure;
  const aItemIdx: Integer; const multiLine: Boolean): string;
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
        if multiLine then
          s := format(#13'"%s"',[GetTagItemName(fm3[ref^.refIndex]^,0)])
        else
          s := format(' name="%s"',[GetTagItemName(fm3[ref^.refIndex]^,0)]);
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
    Result := GetTagItemName(aTag,aItemIdx);
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
        GetTagItemName(pref^,ref_from.rfItemIndex),
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
  Result := treeIDs.Items.AddChildObject(node,GetTagItemName(aTag^,aIdx),aTag);
  Result.ImageIndex := aIdx;
end;

procedure TFAnimListView.EditCHARbyRef(const F: TM3Field);
var
  pTmp: PM3Structure;
  i, mr, l: Integer;
  s: string;
  p: Pointer;
begin
  mr := mrNone;
  pTmp := FM3[Pm3ref(F.fData)^.refIndex];
  if (pTmp^.Tag = CHARTag) and (pTmp^.SpecialType <> sstCharBinary) and (Length(pTmp^.RefFrom) = 1) then
  begin
    with TFEditString.Create(Self) do
    try
      FInitalVal := PChar(pTmp^.Data);
      BResetClick(nil);
      BEditCHARRef.Visible := True;
      mr := ShowModal;
      if mr = mrOK then
      begin
        s := Edit.Text;
        l := pTmp^.ItemCount;
        ResizeStructure(pTmp^,length(s)+1);
        StrPCopy(pTmp^.Data,s);
        if (Length(pTmp^.RefFrom) > 0) and (pTmp^.ItemCount <> l) then
        begin
          for i := 0 to length(pTmp^.RefFrom)-1 do
          with pTmp^.RefFrom[i] do
          begin
            p := FM3[rfTagIndex]^.Data + rfRefFieldOffset;
            Pm3ref_small(p)^.refCount := pTmp^.ItemCount;
          end;
        end;
        FMain.ModelChanged(Self);
        UpdateKeyTagTable;
      end;
    finally
      Free;
    end;
  end;
  if mr = mrRetry then
    EditRefField(F);
end;

procedure TFAnimListView.EditInt8Field(const F: TM3Field);
begin
  with TFEditByte.Create(Self) do
  try
    FInitValue := PUInt8(F.fData)^;
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          PUInt8(F.fData)^ := FCurValue;
          FMain.ModelChanged(Self);
          UpdateKeyTagTable;
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFAnimListView.EditInt16Field(const F: TM3Field);
begin
  with TFEditByte.Create(Self) do
  try
    FInitValue := PUInt16(F.fData)^;
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          PUInt16(F.fData)^ := FCurValue;
          FMain.ModelChanged(Self);
          UpdateKeyTagTable;
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFAnimListView.EditInt32Field(const F: TM3Field);
begin
  with TFEditByte.Create(Self) do
  try
    FInitValue := PUInt32(F.fData)^;
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          PUInt32(F.fData)^ := FCurValue;
          FMain.ModelChanged(Self);
          UpdateKeyTagTable;
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFAnimListView.EditFloatField(const F: TM3Field);
begin
  with TFEditFloat.Create(Self) do
  try
    FInitValue := PSingle(F.fData)^;
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          PSingle(F.fData)^ := StrToFloatDef(Edit.Text,0,FloatDotFormat);
          FMain.ModelChanged(Self);
          UpdateKeyTagTable;
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFAnimListView.EditFlagField(const F: TM3Field);
var
  i: integer;
begin
  with TFEditFlags.Create(Self) do
  try
    case F.fSize of
      1: begin
        FInitValue := pUInt8(F.fData)^;
        for i := 8 to 31 do
          cbValues.CheckEnabled[i] := False;
      end;
      2: begin
        FInitValue := pUInt16(F.fData)^;
        for i := 16 to 31 do
          cbValues.CheckEnabled[i] := False;
      end;
      4: FInitValue := pUInt32(F.fData)^;
    end;
    for i := 0 to 31 do
      if F.fTypeFlagBits[i] <> '' then
        cbValues.Items[i] := F.fTypeFlagBits[i];
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          case F.fSize of
            1: pUInt8(F.fData)^ := FVal8;
            2: pUInt16(F.fData)^ := FVal16;
            4: pUInt32(F.fData)^ := FInitValue;
          end;
          FMain.ModelChanged(Self);
          UpdateKeyTagTable;
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFAnimListView.EditRefField(const F: TM3Field);
begin
  with TFRefEdit.Create(Self) do
  try
    if ShowEditor(FM3,F,FkgRows[FkgSelRow].keyTag^.Index) then
    begin
      FMain.ModelChanged(Self);
      UpdateKeyTagTable;
    end;
  finally
    Free;
  end;
end;

procedure TFAnimListView.EditVEC3asColor(const F: TM3Field);
begin
  with TFEditColor.Create(Self) do
  try
    if ShowEditorVEC3(F.fData) then
    begin
      FMain.ModelChanged(Self);
      UpdateKeyTagTable;
    end;
  finally
    Free;
  end;
end;

procedure TFAnimListView.EditCOLasColor(const F: TM3Field);
begin
  with TFEditColor.Create(Self) do
  try
    if ShowEditorCOL(F.fData) then
    begin
      FMain.ModelChanged(Self);
      UpdateKeyTagTable;
    end;
  finally
    Free;
  end;
end;

function TFAnimListView.kgRowFilterPass(rTitle: string): boolean;
var
  s, srch: string;
begin
  if FkgSearch = '' then Exit(true);
  rTitle := AnsiLowerCase(rTitle);
  srch := FkgSearch;
  Result := false;
  repeat
    s := ExtractWord(srch);
    if Pos(s,rTitle) = 0 then Exit;
  until srch = '';
  Result := True;
end;

function TFAnimListView.kgFrameToCol(const aFrame: Int32): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to length(FkgColToFrame)-1 do
    if FkgColToFrame[i]=aFrame then
      Exit(i);
end;

function TFAnimListView.kgFrameAdd(const aFrame: Int32): Integer;
var
  i, j, len: Integer;
begin
  i := 0;
  len := Length(FkgColToFrame);
  while (i < len) and (aFrame > FkgColToFrame[i]) do
    inc(i);
  Result := i;
  if (len = i) or (FkgColToFrame[i] > aFrame) then
  begin
    SetLength(FkgColToFrame, len+1);
    for j := len downto i+1 do
      FkgColToFrame[j] := FkgColToFrame[j-1];
    FkgColToFrame[i] := aFrame;
  end;
end;

procedure TFAnimListView.ClearKGData;
var
  i: Integer;
begin
  for i := 0 to length(FkgRows)-1 do
  begin
    SetLength(FkgRows[i].frames,0);
  end;
  SetLength(FkgRows,0);
  SetLength(FkgColToFrame,0);
end;

procedure TFAnimListView.DisplayKGData(const STC_idx: Integer);
var
  i, j, k, rowIdx: Integer;
  pFrames: PM3Structure;
begin
  ClearKGData;
  dgAnimationKeyTable.ColCount := 1;
  dgAnimationKeyTable.RowCount := 1;
  FkgFrameLast := -1;
  if (STC_idx < 0) or (STC_idx >= length(FAnimSTCs)) then Exit;
  FkgCurSTCIdx := STC_idx;
  if comboSTC.ItemIndex <> STC_idx then
    comboSTC.ItemIndex := STC_idx;
  with FAnimSTCs[STC_idx] do
  begin
    FkgFrameLast := 0;
    SetLength(FkgRows,length(animIds));
    rowIdx := 0;
    for i := 0 to length(animIds)-1 do
    begin
      FkgRows[rowIdx].animID := animIds[i];
      FkgRows[rowIdx].animInfo := GetAnimId(animIds[i]);
      if Assigned(FkgRows[rowIdx].animInfo) then
      begin
        with FkgRows[rowIdx].animInfo^ do
          FkgRows[rowIdx].animRowTitle := Format('0x%.8x (%d)'#13'%s'#13'%s',
          [animIds[i],animIds[i],GetTagItemName(parentTag^,parentItemIndex,true),ExtraxtBaseFieldName(fieldName)]);
      end
      else
        FkgRows[rowIdx].animRowTitle := Format('0x%.8x (%d)'#13,[animIds[i],animIds[i]]);
      if Assigned(sequenceData[i]) then
      begin
        FkgRows[rowIdx].keyTag := fm3[sequenceData[i]^.keys.refIndex];
        if FkgFrameLast < sequenceData[i]^.fend then
          FkgFrameLast := sequenceData[i]^.fend;
        pFrames := fm3[sequenceData[i]^.frames.refIndex];
        if Assigned(pFrames) then
        begin
          SetLength(FkgRows[rowIdx].frames,pFrames^.ItemCount);
          for j := 0 to pFrames^.ItemCount-1 do
          begin
            FkgRows[rowIdx].frames[j] := pInt32(pFrames^.Data + pFrames^.ItemSize*j)^;
            if FkgFrameLast < FkgRows[rowIdx].frames[j] then
              FkgFrameLast := FkgRows[rowIdx].frames[j];
            kgFrameAdd(FkgRows[rowIdx].frames[j]);
          end;
        end;
      end
      else
        FkgRows[rowIdx].keyTag := nil;
      if Assigned(FkgRows[rowIdx].keyTag) then
      begin
        FkgRows[rowIdx].animRowTitle += ' - ' + FkgRows[rowIdx].keyTag^.StructName;
      end
      else
      begin
        FkgRows[rowIdx].animRowTitle += ' - (not found)';
      end;
      if kgRowFilterPass(FkgRows[rowIdx].animRowTitle) then
        inc(rowIdx)
      else
        SetLength(FkgRows[rowIdx].frames,0);
    end;
    SetLength(FkgRows,rowIdx);
    if sbHideEmptyFrames.Down then
      dgAnimationKeyTable.ColCount := length(FkgColToFrame)+1
    else
      dgAnimationKeyTable.ColCount := FkgFrameLast+2;
    dgAnimationKeyTable.RowCount := length(FkgRows)+1;
  end;
end;

procedure TFAnimListView.SelectFrame(const aRow, aFrameIdx: Integer);
begin
  PanelBottom.Enabled := false;
  lblFullPath.Caption := 'No frame selected';
  FkgSelRow := -1;
  FkgSelFrame := -1;
  FkgKeyTagItemData := nil;
  if (aRow < 0) or (aRow >= length(FkgRows)) then
    Exit;
  with FkgRows[aRow] do
  begin
    if (aFrameIdx < 0) or (aFrameIdx >= Length(frames)) then
      Exit;
    PanelBottom.Enabled := true;

    FkgSelRow := aRow;
    FkgSelFrameIdx := aFrameIdx;
    FkgSelFrame := frames[aFrameIdx];

    dgAnimationKeyTable.Row := aRow+1;
    if sbHideEmptyFrames.Down then
      dgAnimationKeyTable.Col := kgFrameToCol(FkgSelFrame)+1
    else
      dgAnimationKeyTable.Col := FkgSelFrame+1;

    if Assigned(animInfo) then
      lblFullPath.Caption := Format('Frame #%d animating %s - %s',
        [FkgSelFrame,animInfo^.tagPath,ExtraxtBaseFieldName(animInfo^.fieldName)])
    else
      lblFullPath.Caption := Format('Frame #%d, key-frame without property',[FkgSelFrame]);
    sgKeyTagTable.Enabled := false;
    if Assigned(keyTag) and (aFrameIdx < keyTag^.ItemCount) and Structures.GetStructureInfo(keyTag^,true) then
    begin
      sgKeyTagTable.Enabled := true;
      FkgKeyTagItemData := (keyTag^.Data + keyTag^.ItemSize*aFrameIdx);
      UpdateKeyTagTable;
    end;
  end;
end;

procedure TFAnimListView.UpdateKeyTagTable;
var
  i,r,c: Integer;
  s, ref: string;
begin
  if (FkgSelRow < 0) or (FkgSelRow >= length(FkgRows)) or (FkgKeyTagItemData = nil) then
  begin
    sgKeyTagTable.RowCount := 1;
    Exit;
  end;
  with FkgRows[FkgSelRow] do
  begin
    sgKeyTagTable.RowCount := length(keyTag^.ItemFields)+1;
    for i := 0 to length(keyTag^.ItemFields)-1 do
      with keyTag^.ItemFields[i] do
      begin
        fData := FkgKeyTagItemData + fOffset;
        sgKeyTagTable.Cells[COL_Name,i+1] := RepeatStr('- ',fSubLevel)+fGroupName+fName;

        sgKeyTagTable.Cells[COL_Type,i+1] := fTypeName;
        ref := '';
        case fType of
          ftSubStruct:
            sgKeyTagTable.Cells[COL_Info,i+1] := fTypeInfo;
          ftRef,ftRefSmall:
            with Pm3ref_small(fData)^ do
            begin
              if fRefTo <> '' then
                sgKeyTagTable.Cells[COL_Info,i+1] := 'Should reference ' + fRefTo;
              if (refCount > 0) and (refIndex > 0) and (refIndex < FM3.TagCount) then
                ref := GetTreeTagName(FM3[refIndex]^);
            end
          else
            begin
              s := '';
              if fTypeFlag then s += 'Flags; '
              else if fDefault <> '' then s += 'Default = "'+fDefault+'"; '
              else if fExpected <> '' then s += 'Expected = "'+fExpected+'";';
              sgKeyTagTable.Cells[COL_Info,i+1] := s;
            end;
        end;
        if ref = '' then
          sgKeyTagTable.Cells[COL_Value,i+1] := FieldValToStr(keyTag^.ItemFields[i])
        else
          sgKeyTagTable.Cells[COL_Value,i+1] := FieldValToStr(keyTag^.ItemFields[i]) + ' -> ' + ref;
      end;
  end;
end;

procedure TFAnimListView.ShowEditor(const M3File: TM3File);
begin
  dgAnimationKeyTable.ColWidths[0]:=176;

  FM3 := M3File;
  ResetAnimView;
  Show;
end;

procedure TFAnimListView.ResetAnimView;
begin
  ParseAnimData;
  ResetSTCList;
  ResetIDTree;
  FkgCurSTCIdx := -1;
  DisplayKGData(0);
  SelectFrame(0,0);
end;

end.

