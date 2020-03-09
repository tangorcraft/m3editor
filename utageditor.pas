unit uTagEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Grids, ComCtrls, ustructures, uM3File, uEditString, uEditInteger,
  uEditFlags, uEditWord, uEditByte, uEditFloat;

type

  { TFTagEditor }

  TFTagEditor = class(TForm)
    btnPrev: TButton;
    btnNext: TButton;
    lblItemIndex: TLabel;
    MemoDesc: TMemo;
    PanelNavi: TPanel;
    PanelMain: TPanel;
    PanelBottom: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TableView: TStringGrid;
    treeTags: TTreeView;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure TableViewDblClick(Sender: TObject);
    procedure treeTagsSelectionChanged(Sender: TObject);
  private
    FM3File: TM3File;
    FM3Struct: PM3Structure;
    FTagItemDisplayRange: Boolean;
    FTagItemIdxFirst: Integer;
    FTagItemIdxLast: Integer;

    procedure SelectStructure(const S: PM3Structure);

    procedure UpdateItemLabel;

    procedure DisplayCHAR;
    procedure DisplayStructure;

    procedure EditCHAR;
    procedure EditInt8Field(const F: TM3Field);
    procedure EditInt16Field(const F: TM3Field);
    procedure EditInt32Field(const F: TM3Field);
    procedure EditFloatField(const F: TM3Field);
    procedure EditFlagField(const F: TM3Field);
  public
    procedure ShowEditor(const M3File: TM3File; const Modal: boolean = false);
    procedure ResetTagTree;
    procedure UpdateTagTree;
    procedure UpdateItemTable;
  end;

var
  FTagEditor: TFTagEditor;

implementation

uses
  umain;

const
  COL_Name = 0;
  COL_Type = 1;
  COL_Info = 2;
  COL_Value = 3;

function IndexZeros(const Index, ZeroCount: integer): string;
begin
  Result := IntToStr(Index);
  while length(Result) < ZeroCount do
    Result := '0' + Result;
end;

function FieldValToStr(const F: TM3Field): string;
var
  i: integer;
  v: UInt32;
begin
  case F.fType of
    ftBinary: Result := Format('{Binary Data, size=%d}',[F.fSize]);
    ftUInt8: Result := Format('%d (0x%s)',[pUInt8(F.fData)^,IntToHex(pUInt8(F.fData)^,2)]);
    ftUInt16: Result := Format('%d (0x%s)',[pUInt16(F.fData)^,IntToHex(pUInt16(F.fData)^,4)]);
    ftUInt32: Result := Format('%d (0x%0:.8x)',[pUInt32(F.fData)^]);
    ftInt8: Result := Format('%d (0x%s)',[pInt8(F.fData)^,IntToHex(pUInt8(F.fData)^,2)]);
    ftInt16: Result := Format('%d (0x%s)',[pInt16(F.fData)^,IntToHex(pUInt16(F.fData)^,2)]);
    ftInt32: Result := Format('%d (0x%0:.8x)',[pInt32(F.fData)^]);
    ftFloat: Result := Format('%s (0x%.8x)',[M3FloatToStr(PSingle(F.fData)^),pUInt32(F.fData)^]);
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
    Result:=Format('%d: %s "%s"',[tag.Index,tag.StructName,s]);
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

{$R *.lfm}

{ TFTagEditor }

procedure TFTagEditor.treeTagsSelectionChanged(Sender: TObject);
begin
  SelectStructure(treeTags.Selected.Data);
end;

procedure TFTagEditor.SelectStructure(const S: PM3Structure);
begin
  if S <> nil then
    FM3Struct := PM3Structure(treeTags.Selected.Data);
  if FM3Struct <> nil then
  begin
    Structures.GetStructureInfo(FM3Struct^);
    MemoDesc.Text := Trim(FM3Struct^.Description);
    FTagItemIdxFirst := 0;
    FTagItemDisplayRange := False;
    PanelNavi.Enabled := True;
    UpdateItemTable;
  end;
end;

procedure TFTagEditor.btnPrevClick(Sender: TObject);
begin
  if FTagItemDisplayRange then
  begin
    if FTagItemIdxFirst > 50 then
      dec(FTagItemIdxFirst,50);
    FTagItemIdxLast := FTagItemIdxFirst + 50;
    if FTagItemIdxLast >= FM3Struct^.ItemCount then
      FTagItemIdxLast := FM3Struct^.ItemCount - 1;
  end
  else
  begin
    if FTagItemIdxFirst > 0 then
      dec(FTagItemIdxFirst);
  end;
  UpdateItemTable;
end;

procedure TFTagEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFTagEditor.FormDestroy(Sender: TObject);
begin
  FMain.FreeTagEditor;
end;

procedure TFTagEditor.TableViewDblClick(Sender: TObject);
var
  idx: integer;
begin
  if TableView.Col <> COL_Value then Exit;
  if (FM3Struct^.StructName = 'CHAR') and (TableView.Row = 1) then
  begin
    EditCHAR;
    Exit;
  end;

  idx := TableView.Row - 1;
  if (idx >= 0) and (idx < length(FM3Struct^.ItemFields)) then
  with FM3Struct^.ItemFields[idx] do
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
    ftSubStruct: ShowMessageFmt('Editor for "%s" structure in not implemented.',[fTypeName]);
  end;
end;

procedure TFTagEditor.btnNextClick(Sender: TObject);
begin
  if FTagItemDisplayRange then
  begin
    if (FTagItemIdxFirst + 50) < FM3Struct^.ItemCount then
      Inc(FTagItemIdxFirst,50);
    FTagItemIdxLast := FTagItemIdxFirst + 50;
    if FTagItemIdxLast >= FM3Struct^.ItemCount then
      FTagItemIdxLast := FM3Struct^.ItemCount - 1;
  end
  else
  begin
    if (FTagItemIdxFirst + 1) < FM3Struct^.ItemCount then
      Inc(FTagItemIdxFirst);
  end;
  UpdateItemTable;
end;

procedure TFTagEditor.UpdateItemLabel;
begin
  if FTagItemDisplayRange then
    lblItemIndex.Caption := Format('(%d-%d)',[FTagItemIdxFirst,FTagItemIdxLast])
  else
    lblItemIndex.Caption := Format('%d',[FTagItemIdxFirst])
end;

procedure TFTagEditor.UpdateItemTable;
var
  r,c: Integer;
begin
  treeTags.Selected.DeleteChildren;
  r := TableView.Row;
  c := TableView.Col;
  if FM3Struct^.StructName = 'CHAR' then DisplayCHAR
  else
    DisplayStructure;
  UpdateItemLabel;
  TableView.Row := r;
  TableView.Col := c;
end;

procedure TFTagEditor.DisplayCHAR;
begin
  PanelNavi.Enabled := False;
  TableView.RowCount := 2;
  TableView.Cells[COL_Name,1] := FM3Struct^.StructName;
  TableView.Cells[COL_Type,1] := 'string';
  TableView.Cells[COL_Info,1] := '';
  TableView.Cells[COL_Value,1] := PChar(FM3Struct^.Data);
end;

procedure TFTagEditor.DisplayStructure;
var
  i, off: integer;
  s: string;
begin
  off := FM3Struct^.ItemSize * FTagItemIdxFirst;
  TableView.RowCount := length(FM3Struct^.ItemFields)+1;
  for i := 0 to length(FM3Struct^.ItemFields)-1 do
  with FM3Struct^.ItemFields[i] do
  begin
    fData := FM3Struct^.Data + fOffset + off;
    TableView.Cells[COL_Name,i+1] := RepeatStr('- ',fSubLevel)+fGroupName+fName;
    TableView.Cells[COL_Type,i+1] := fTypeName;
    if fType = ftSubStruct then
    begin
      TableView.Cells[COL_Info,i+1] := fTypeInfo;
      if fTypeName = 'Reference' then
      with Pm3ref(fData)^ do
      begin
        if (refCount > 0) and (refIndex > 0) and (refIndex < FM3File.TagCount) then
          treeTags.Items.AddChildObject(treeTags.Selected,fName+' -> '+GetTreeTagName(FM3File[refIndex]^),FM3File[refIndex]);
      end
      else if fTypeName = 'SmallReference' then
      with Pm3ref_small(fData)^ do
      begin
        if (refCount > 0) and (refIndex > 0) and (refIndex < FM3File.TagCount) then
          treeTags.Items.AddChildObject(treeTags.Selected,fName+' -> '+GetTreeTagName(FM3File[refIndex]^),FM3File[refIndex]);
      end
    end
    else
    begin
      s := '';
      if fTypeFlag then s += 'Flags; '
      else if fDefault <> '' then s += 'Default = "'+fDefault+'"; '
      else if fExpected <> '' then s += 'Expected = "'+fExpected+'";';
      TableView.Cells[COL_Info,i+1] := s;
    end;
    TableView.Cells[COL_Value,i+1] := FieldValToStr(FM3Struct^.ItemFields[i]);
  end;
end;

procedure TFTagEditor.EditCHAR;
var
  s: string;
  i: Integer;
  p: Pointer;
begin
  s := PChar(FM3Struct^.Data);
  with TFEditString.Create(Self) do
  try
    FInitalVal := s;
    BResetClick(nil);
    if ShowModal = mrOK then
    begin
      s := Edit.Text;
      ResizeStructure(FM3Struct^,length(s)+1);
      p := FM3Struct^.Data;
      for i := 1 to length(s) do
      begin
        PAnsiChar(p)^ := s[i];
        inc(p);
      end;
      PAnsiChar(p)^ := #0;
      FMain.ModelChanged(Self);
    end;
  finally
    Free;
  end;
  TableView.Cells[COL_Value,1] := PChar(FM3Struct^.Data);
  UpdateTagTree;
end;

procedure TFTagEditor.EditInt8Field(const F: TM3Field);
begin
  with TFEditByte.Create(Self) do
  try
    FInitValue := PUInt8(F.fData)^;
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          PUInt8(F.fData)^ := FCurValue;
          UpdateItemTable;
          FMain.ModelChanged(Self);
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.EditInt16Field(const F: TM3Field);
begin
  with TFEditWord.Create(Self) do
  try
    FInitValue := PUInt16(F.fData)^;
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          PUInt16(F.fData)^ := FCurValue;
          UpdateItemTable;
          FMain.ModelChanged(Self);
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.EditInt32Field(const F: TM3Field);
begin
  with TFEditInteger.Create(Self) do
  try
    FInitValue := PUInt32(F.fData)^;
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          PUInt32(F.fData)^ := FCurValue;
          UpdateItemTable;
          FMain.ModelChanged(Self);
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.EditFloatField(const F: TM3Field);
begin
  with TFEditFloat.Create(Self) do
  try
    FInitValue := PSingle(F.fData)^;
    BResetClick(nil);
    case ShowModal of
      mrOK:
        begin
          PSingle(F.fData)^ := StrToFloatDef(Edit.Text,0);
          UpdateItemTable;
          FMain.ModelChanged(Self);
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.EditFlagField(const F: TM3Field);
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
          UpdateItemTable;
          FMain.ModelChanged(Self);
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.ShowEditor(const M3File: TM3File; const Modal: boolean);
begin
  FM3File := M3File;

  ResetTagTree;

  if Modal then
    ShowModal
  else
    Show;
end;

procedure TFTagEditor.ResetTagTree;
var
  i: Integer;
  s: string;
begin
  treeTags.Items.Clear;
  for i := 0 to FM3File.TagCount-1 do
  begin
    if FM3File[i] = FM3Struct then
      treeTags.Select(treeTags.Items.AddObject(nil,GetTreeTagName(FM3File[i]^),FM3File[i]))
    else
      treeTags.Items.AddObject(nil,GetTreeTagName(FM3File[i]^),FM3File[i]);
  end;
end;

procedure TFTagEditor.UpdateTagTree;
var
  pre, s: string;
  i: integer;
begin
  with treeTags.Items.GetEnumerator do
  try
    while MoveNext do
      if (Current.Data <> nil) and (PM3Structure(Current.Data)^.StructName='CHAR') then
      with PM3Structure(Current.Data)^ do
      begin
        i := Pos(' -> ',Current.Text);
        if (i<>0) and (Current.Parent <> nil) then
          pre := copy(Current.Text,1,i+3)
        else
          pre := '';
        s := PChar(Data);
        Current.Text := pre + Format('%d: %s "%s"',[Index,StructName,s]);
      end;
  finally
    Free;
  end;
end;

end.

