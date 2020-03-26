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
unit uTagEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Grids, ComCtrls, ustructures, uM3File, uEditString, uEditInteger,
  uEditFlags, uEditWord, uEditByte, uEditFloat, uRefEdit, uNewTag, uColorEditor;

type

  { TFTagEditor }

  TFTagEditor = class(TForm)
    btnInsertTag: TSpeedButton;
    btnDelTag: TSpeedButton;
    btnDelTagCascade: TSpeedButton;
    btnMoveItemDown: TSpeedButton;
    btnMoveTagDown: TSpeedButton;
    btnMoveItemUp: TSpeedButton;
    btnDuplicateItem: TSpeedButton;
    btnAppendItem: TSpeedButton;
    btnDelItem: TSpeedButton;
    btnMoveTagUp: TSpeedButton;
    btnTagEdit: TSpeedButton;
    lblItemIndex: TLabel;
    MemoDesc: TMemo;
    PanelLeft: TPanel;
    PanelNavi: TPanel;
    PanelMain: TPanel;
    PanelBottom: TPanel;
    btnPrev: TSpeedButton;
    btnNext: TSpeedButton;
    btnAppendTag: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TableView: TStringGrid;
    treeTags: TTreeView;
    procedure btnAppendTagClick(Sender: TObject);
    procedure btnInsertTagClick(Sender: TObject);
    procedure btnDelTagClick(Sender: TObject);
    procedure btnDelTagCascadeClick(Sender: TObject);
    procedure btnAppendItemClick(Sender: TObject);
    procedure btnDelItemClick(Sender: TObject);
    procedure btnDuplicateItemClick(Sender: TObject);
    procedure btnMoveItemDownClick(Sender: TObject);
    procedure btnMoveItemUpClick(Sender: TObject);
    procedure btnMoveTagDownClick(Sender: TObject);
    procedure btnMoveTagUpClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure btnTagEditClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure TableViewClick(Sender: TObject);
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
    procedure UpdateDescription;

    procedure DisplayCHAR;
    procedure DisplayStructure;

    procedure EditCHAR;
    procedure EditInt8Field(const F: TM3Field);
    procedure EditInt16Field(const F: TM3Field);
    procedure EditInt32Field(const F: TM3Field);
    procedure EditFloatField(const F: TM3Field);
    procedure EditFlagField(const F: TM3Field);
    procedure EditRefField(const F: TM3Field);
    procedure EditVEC3asColor(const F: TM3Field);
  public
    procedure ShowEditor(const M3File: TM3File);
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
  if (S <> nil) and Assigned(treeTags.Selected) then
    FM3Struct := PM3Structure(treeTags.Selected.Data);
  if FM3Struct <> nil then
  begin
    Structures.GetStructureInfo(FM3Struct^);
    UpdateDescription;
    FTagItemIdxFirst := 0;
    FTagItemDisplayRange := False;
    PanelNavi.Enabled := True;
    UpdateItemTable;
  end
  else
  begin
    PanelNavi.Enabled := False;
    TableView.RowCount := 1;
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

procedure TFTagEditor.btnTagEditClick(Sender: TObject);
begin
  if FM3Struct = nil then Exit;
  MessageDlg(
    'Change tag ID',
    'Changing tag ID will delete all current tag data',
    mtWarning, [mbOK], 0
  );
  with TFNewTag.Create(Self) do
  try
    if ShowEditTag(FM3File,FM3Struct^.Index) then
    begin
      FMain.ModelChanged(Self);
      ResetTagTree;
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFTagEditor.FormDestroy(Sender: TObject);
begin
  FMain.FreeTagEditor;
end;

procedure TFTagEditor.TableViewClick(Sender: TObject);
begin
  UpdateDescription;
end;

procedure TFTagEditor.TableViewDblClick(Sender: TObject);
var
  idx: integer;
begin
  if TableView.Col <> COL_Value then Exit;
  if (FM3Struct^.StructName = 'CHAR') and (FM3Struct^.SpecialType <> sstCharBinary) and (TableView.Row = 1) then
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
    else
      begin
        if Copy(fTypeName,1,4) = 'VEC3' then
          EditVEC3asColor(FM3Struct^.ItemFields[idx])
        else
          ShowMessageFmt('Editor for "%s" structure in not implemented.',[fTypeName]);
      end;
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

procedure TFTagEditor.btnDuplicateItemClick(Sender: TObject);
begin
  FTagItemIdxFirst := DuplicateStructureItem(FM3Struct^,FTagItemIdxFirst);
  FMain.ModelChanged(Self);
  UpdateTagTree;
  UpdateItemTable;
end;

procedure TFTagEditor.btnMoveItemDownClick(Sender: TObject);
begin
  if FTagItemIdxFirst < (FM3Struct^.ItemCount-1) then
  begin
    ExchangeStructureItems(FM3Struct^,FTagItemIdxFirst,FTagItemIdxFirst+1);
    inc(FTagItemIdxFirst);
    UpdateItemTable;
  end;
end;

procedure TFTagEditor.btnMoveItemUpClick(Sender: TObject);
begin
  if FTagItemIdxFirst > 0 then
  begin
    ExchangeStructureItems(FM3Struct^,FTagItemIdxFirst,FTagItemIdxFirst-1);
    dec(FTagItemIdxFirst);
    UpdateItemTable;
  end;
end;

procedure TFTagEditor.btnMoveTagDownClick(Sender: TObject);
begin
  if FM3Struct = nil then Exit;
  FM3File.MoveTagDown(FM3Struct^.Index);
  FMain.ModelChanged(Self);
  ResetTagTree;
end;

procedure TFTagEditor.btnMoveTagUpClick(Sender: TObject);
begin
  if FM3Struct = nil then Exit;
  FM3File.MoveTagUp(FM3Struct^.Index);
  FMain.ModelChanged(Self);
  ResetTagTree;
end;

procedure TFTagEditor.btnAppendItemClick(Sender: TObject);
begin
  ResizeStructure(FM3Struct^,FM3Struct^.ItemCount+1);
  CopyStructureItem(FM3Struct^,FTagItemIdxFirst,FM3Struct^.ItemCount-1);
  FTagItemIdxFirst := FM3Struct^.ItemCount-1;
  FMain.ModelChanged(Self);
  UpdateTagTree;
  UpdateItemTable;
end;

procedure TFTagEditor.btnInsertTagClick(Sender: TObject);
begin
  with TFNewTag.Create(Self) do
  try
    if ShowAddTag(FM3File,FM3Struct^.Index+1) then
    begin
      FMain.ModelChanged(Self);
      ResetTagTree;
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.btnAppendTagClick(Sender: TObject);
begin
  with TFNewTag.Create(Self) do
  try
    if ShowAddTag(FM3File,FM3File.TagCount) then
    begin
      FMain.ModelChanged(Self);
      ResetTagTree;
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.btnDelTagClick(Sender: TObject);
begin
  if FM3Struct = nil then Exit;
  if MessageDlg(
    'Tag delete',
    'You sure you want to delete tag '+GetTreeTagName(FM3Struct^)+'?',
    mtConfirmation, mbYesNo, 0
  ) = mrYes then
  begin
    FM3File.DeleteTag(FM3Struct^.Index);
    FM3Struct := nil;
    FMain.ModelChanged(Self);
    ResetTagTree;
  end;
end;

procedure TFTagEditor.btnDelTagCascadeClick(Sender: TObject);
begin
  if FM3Struct = nil then Exit;
  if MessageDlg(
    'Tag delete',
    'You sure you want to delete tag '+GetTreeTagName(FM3Struct^)+#13+
    'and all tags that only referenced by this tag?',
    mtConfirmation, mbYesNo, 0
  ) = mrYes then
  begin
    FM3File.DeleteTagCascade(FM3Struct^.Index);
    FM3Struct := nil;
    FMain.ModelChanged(Self);
    ResetTagTree;
  end;
end;

procedure TFTagEditor.btnDelItemClick(Sender: TObject);
begin
  if MessageDlg(
       'Delete item',
       'Delete item at index ['+IntToStr(FTagItemIdxFirst)+']',
       mtConfirmation, mbYesNo, 0
  ) <> mrYes then Exit;
  DeleteStructureItem(FM3Struct^,FTagItemIdxFirst);
  if FTagItemIdxFirst > (FM3Struct^.ItemCount-1) then
    FTagItemIdxFirst := (FM3Struct^.ItemCount-1);
  FMain.ModelChanged(Self);
  UpdateTagTree;
  UpdateItemTable;
end;

procedure TFTagEditor.UpdateItemLabel;
begin
  if FM3Struct^.ItemCount = 0 then
    lblItemIndex.Caption := '[No items]'
  else if FTagItemDisplayRange then
    lblItemIndex.Caption := Format('[%d-%d]',[FTagItemIdxFirst,FTagItemIdxLast])
  else
    lblItemIndex.Caption := Format('[%d]',[FTagItemIdxFirst])
end;

procedure TFTagEditor.UpdateDescription;
var
  s: string;
  i, lvl: integer;
begin
  if FM3Struct = nil then Exit;
  i := TableView.Row - 1;
  s := '';
  if (i >= 0) and (i < length(FM3Struct^.ItemFields)) then
  begin
    lvl := FM3Struct^.ItemFields[i].fSubLevel;
    while (lvl >= 0) and (i >= 0) do
    begin
      with FM3Struct^.ItemFields[i] do
        s := Format('%s:'#13'%s'#13,[fGroupName+fName,fHint]) + s;
      dec(lvl);
      while (i >= 0) and (FM3Struct^.ItemFields[i].fSubLevel > lvl) do
        dec(i);
    end;
  end;
  MemoDesc.Text := format('Tag %s:'#13'%s'#13'%s',[FM3Struct^.StructName,FM3Struct^.Description,s]);
end;

procedure TFTagEditor.UpdateItemTable;
var
  r,c: Integer;
begin
  if FM3Struct = nil then
  begin
    PanelNavi.Enabled := False;
    TableView.RowCount := 1;
    Exit;
  end;
  if Assigned(treeTags.Selected) then
    treeTags.Selected.DeleteChildren;
  r := TableView.Row;
  c := TableView.Col;
  if (FM3Struct^.StructName = 'CHAR') and (FM3Struct^.SpecialType <> sstCharBinary) then DisplayCHAR
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
  if FM3Struct^.ItemCount = 0 then
  begin
    TableView.RowCount := 1;
    Exit;
  end;
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
      StrPCopy(FM3Struct^.Data,s);
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
          PSingle(F.fData)^ := StrToFloatDef(Edit.Text,0,FloatDotFormat);
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
    if ShowEditor(FM3File,F,FM3Struct^.Index) then
    begin
      FMain.ModelChanged(Self);
      UpdateItemTable;
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.EditVEC3asColor(const F: TM3Field);
begin
  with TFEditColor.Create(Self) do
  try
    if ShowEditor(F.fData) then
    begin
      FMain.ModelChanged(Self);
      UpdateItemTable;
    end;
  finally
    Free;
  end;
end;

procedure TFTagEditor.ShowEditor(const M3File: TM3File);
begin
  FM3File := M3File;

  ResetTagTree;
  SelectStructure(nil);

  Show;
end;

procedure TFTagEditor.ResetTagTree;
var
  i, idx: Integer;
begin
  idx := FTagItemIdxFirst;
  treeTags.Items.Clear;
  for i := 0 to FM3File.TagCount-1 do
  begin
    if FM3File[i] = FM3Struct then
    begin
      treeTags.Select(treeTags.Items.AddObject(nil,GetTreeTagName(FM3File[i]^),FM3File[i]));
      FTagItemIdxFirst := idx;
    end
    else
      treeTags.Items.AddObject(nil,GetTreeTagName(FM3File[i]^),FM3File[i]);
  end;
  if not Assigned(treeTags.Selected) then
    treeTags.Select(treeTags.Items.GetFirstNode);
  UpdateItemTable;
end;

procedure TFTagEditor.UpdateTagTree;
var
  pre: string;
  i: integer;
begin
  with treeTags.Items.GetEnumerator do
  try
    while MoveNext do
      if (Current.Data <> nil) then
      with PM3Structure(Current.Data)^ do
      begin
        i := Pos(' -> ',Current.Text);
        if (i<>0) and (Current.Parent <> nil) then
          pre := copy(Current.Text,1,i+3)
        else
          pre := '';
        Current.Text := pre + GetTreeTagName(PM3Structure(Current.Data)^);
      end;
  finally
    Free;
  end;
end;

end.

