unit uTagEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Grids, ComCtrls, ustructures, uM3File, uEditString, uEditInteger,
  uEditFlags, uEditWord, uEditByte, uEditFloat, uRefEdit;

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
    procedure JumpToStruct(const Ref: TM3RefFrom);

    procedure UpdateItemLabel;

    procedure DisplayCHAR;
    procedure DisplayStructure;

    procedure EditCHAR;
    procedure EditInt8Field(const F: TM3Field);
    procedure EditInt16Field(const F: TM3Field);
    procedure EditInt32Field(const F: TM3Field);
    procedure EditFloatField(const F: TM3Field);
    procedure EditFlagField(const F: TM3Field);
    procedure EditRefField(const F: TM3Field);
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
  umain, uCommon;

const
  COL_Name = 0;
  COL_Type = 1;
  COL_Info = 2;
  COL_Value = 3;

{$R *.lfm}

{ TFTagEditor }

procedure TFTagEditor.treeTagsSelectionChanged(Sender: TObject);
begin
  if Assigned(treeTags.Selected) then
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

procedure TFTagEditor.JumpToStruct(const Ref: TM3RefFrom);
begin
  if (Ref.rfTagIndex >= 0) and (Ref.rfTagIndex < FM3File.TagCount) and
    (MessageDlg(
      'Jump to',
      Format('Jump to "%d: %s [%d]"?',[FM3File[Ref.rfTagIndex]^.Index, FM3File[Ref.rfTagIndex]^.StructName, Ref.rfItemIndex]),
      mtConfirmation,
      mbYesNo,
      0
    ) = mrYes)
  then
  begin
    with treeTags.Items.GetEnumerator do
    try
      while MoveNext do
      if (Current.Level = 0)and(PM3Structure(Current.Data)^.Index = Ref.rfTagIndex) then
      begin
        treeTags.Select(Current);
        if (Ref.rfItemIndex >= 0) and (Ref.rfItemIndex < PM3Structure(Current.Data)^.ItemCount) then
          FTagItemIdxFirst := Ref.rfItemIndex;
        UpdateItemTable;
        TableView.Row := Ref.frFieldRow;
        Break;
      end;
    finally
      Free;
    end;
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
    ftRef, ftRefSmall: EditRefField(FM3Struct^.ItemFields[idx]);
    else ShowMessageFmt('Editor for "%s" structure in not implemented.',[fTypeName]);
  end;
  idx := idx - length(FM3Struct^.ItemFields);
  if (idx >= 0) and (idx < length(FM3Struct^.RefFrom)) then
    JumpToStruct(FM3Struct^.RefFrom[idx]);
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
    lblItemIndex.Caption := Format('[%d-%d]',[FTagItemIdxFirst,FTagItemIdxLast])
  else
    lblItemIndex.Caption := Format('[%d]',[FTagItemIdxFirst])
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
var
  i: integer;
  s: string;
begin
  PanelNavi.Enabled := False;
  s:=PChar(FM3Struct^.Data);
  if (length(s)<>(FM3Struct^.ItemCount-1)) and
     (MessageDlg(
       'Size mismatch',Format('CHAR tag size (%d) is different from actual string length (%d)!'#13'Correct tag size?',[FM3Struct^.ItemCount,length(s)+1]),
       mtWarning, mbYesNo,0
     ) = mrYes)
  then
    begin
      FM3Struct^.ItemCount := length(s)+1;
      FMain.ModelChanged(Self);
      UpdateTagTree;
    end;

  TableView.RowCount := 2 + length(FM3Struct^.RefFrom);
  TableView.Cells[COL_Name,1] := FM3Struct^.StructName;
  TableView.Cells[COL_Type,1] := Format('string (%d)',[length(s)]);
  TableView.Cells[COL_Info,1] := 'size (with terminating 0) = '+IntToStr(FM3Struct^.ItemCount);
  TableView.Cells[COL_Value,1] := s;

  for i := 0 to length(FM3Struct^.RefFrom)-1 do
  with FM3Struct^.RefFrom[i] do
  begin
    TableView.Cells[COL_Name,2+i] := '';
    TableView.Cells[COL_Type,2+i] := 'Referenced from';
    TableView.Cells[COL_Info,2+i] := '';
    TableView.Cells[COL_Value,2+i] := rfName;
  end;
end;

procedure TFTagEditor.DisplayStructure;
var
  i, off: integer;
  s, ref: string;
begin
  off := FM3Struct^.ItemSize * FTagItemIdxFirst;
  TableView.RowCount := length(FM3Struct^.ItemFields)+length(FM3Struct^.RefFrom)+1;
  for i := 0 to length(FM3Struct^.ItemFields)-1 do
  with FM3Struct^.ItemFields[i] do
  begin
    fData := FM3Struct^.Data + fOffset + off;
    TableView.Cells[COL_Name,i+1] := RepeatStr('- ',fSubLevel)+fGroupName+fName;

    TableView.Cells[COL_Type,i+1] := fTypeName;
    ref := '';
    case fType of
      ftSubStruct:
        TableView.Cells[COL_Info,i+1] := fTypeInfo;
      ftRef,ftRefSmall:
        with Pm3ref_small(fData)^ do
        begin
          if fRefTo <> '' then
            TableView.Cells[COL_Info,i+1] := 'Should reference ' + fRefTo;
          if (refCount > 0) and (refIndex > 0) and (refIndex < FM3File.TagCount) then
          begin
            ref := GetTreeTagName(FM3File[refIndex]^);
            treeTags.Items.AddChildObject(treeTags.Selected,fName+' -> '+ref,FM3File[refIndex]);
          end;
        end
      else
        begin
          s := '';
          if fTypeFlag then s += 'Flags; '
          else if fDefault <> '' then s += 'Default = "'+fDefault+'"; '
          else if fExpected <> '' then s += 'Expected = "'+fExpected+'";';
          TableView.Cells[COL_Info,i+1] := s;
        end;
    end;
    if ref = '' then
      TableView.Cells[COL_Value,i+1] := FieldValToStr(FM3Struct^.ItemFields[i])
    else
      TableView.Cells[COL_Value,i+1] := FieldValToStr(FM3Struct^.ItemFields[i]) + ' -> ' + ref;
  end;
  off := length(FM3Struct^.ItemFields)+1;
  for i := 0 to length(FM3Struct^.RefFrom)-1 do
  begin
    TableView.Cells[COL_Name,off+i] := '';
    TableView.Cells[COL_Type,off+i] := 'Referenced from';
    TableView.Cells[COL_Info,off+i] := '';
    TableView.Cells[COL_Value,off+i] := FM3Struct^.RefFrom[i].rfName;
  end;
end;

procedure TFTagEditor.EditCHAR;
var
  s: string;
  i, l: Integer;
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
      l := FM3Struct^.ItemCount;
      ResizeStructure(FM3Struct^,length(s)+1);
      p := FM3Struct^.Data;
      for i := 1 to length(s) do
      begin
        PAnsiChar(p)^ := s[i];
        inc(p);
      end;
      PAnsiChar(p)^ := #0;
      if (Length(FM3Struct^.RefFrom) > 0) and (FM3Struct^.ItemCount <> l) and
         (MessageDlg(
           'References update',
           'References to this tag should be updated with new size of this tag.'#13+
           'This can be done automatically, but this feature is experimental.'#13+
           'Update references?',
           mtConfirmation,
           mbYesNo,
           0
         ) = mrYes)
      then
      begin
        for i := 0 to length(FM3Struct^.RefFrom)-1 do
        with FM3Struct^.RefFrom[i] do
        begin
          p := FM3File[rfTagIndex]^.Data + rfRefFieldOffset;
          Pm3ref_small(p)^.refCount := FM3Struct^.ItemCount;
        end;
      end;
      FMain.ModelChanged(Self);
    end;
  finally
    Free;
  end;
  UpdateItemTable;
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
          FMain.ModelChanged(Self);
          UpdateItemTable;
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
          FMain.ModelChanged(Self);
          UpdateItemTable;
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
          FMain.ModelChanged(Self);
          UpdateItemTable;
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
          FMain.ModelChanged(Self);
          UpdateItemTable;
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
          FMain.ModelChanged(Self);
          UpdateItemTable;
        end;
      mrRetry: EditFlagField(F);
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.EditRefField(const F: TM3Field);
begin
  with TFRefEdit.Create(Self) do
  try
    ShowEditor(FM3File,F,FM3Struct^.Index);
    FMain.ModelChanged(Self);
    UpdateItemTable;
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

