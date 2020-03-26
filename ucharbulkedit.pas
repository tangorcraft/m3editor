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
unit uCHARBulkEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uM3File, ustructures;

type

  { TFCHARBulkEdit }

  TFCHARBulkEdit = class(TForm)
    BOk: TButton;
    BCancel: TButton;
    BReset: TButton;
    cbAutoUpdate: TCheckBox;
    MemoCHAR: TMemo;
    procedure BResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FInitCharTags: TStringList;

    procedure EditCHAR(var Struct: TM3Structure; const M3: TM3File; const Val: string);
  public
    procedure ShowEditor(const M3: TM3File);
  end;

var
  FCHARBulkEdit: TFCHARBulkEdit;

implementation

uses
  umain;

{$R *.lfm}

{ TFCHARBulkEdit }

procedure TFCHARBulkEdit.BResetClick(Sender: TObject);
begin
  MemoCHAR.Lines := FInitCharTags;
end;

procedure TFCHARBulkEdit.FormCreate(Sender: TObject);
begin
  FInitCharTags := TStringList.Create;
end;

procedure TFCHARBulkEdit.FormDestroy(Sender: TObject);
begin
  FInitCharTags.Free;
end;

procedure TFCHARBulkEdit.EditCHAR(var Struct: TM3Structure; const M3: TM3File;
  const Val: string);
var
  i, l: Integer;
  p: Pointer;
begin
  if (Struct.StructName <> 'CHAR') and (Struct.SpecialType = sstCharBinary) then
  begin
    FMain.Log('CHAR bulk edit: Error: tag at index %d is not "CHAR" tag or is a binary CHAR tag.');
    Exit;
  end;
  l := Struct.ItemCount;
  ResizeStructure(Struct,length(Val)+1);
  StrPCopy(Struct.Data,Val);
  if (Length(Struct.RefFrom) > 0) and (Struct.ItemCount <> l) and (cbAutoUpdate.Checked) then
  begin
    for i := 0 to length(Struct.RefFrom)-1 do
    with Struct.RefFrom[i] do
    begin
      p := M3[rfTagIndex]^.Data + rfRefFieldOffset;
      Pm3ref_small(p)^.refCount := Struct.ItemCount;
    end;
  end;
  FMain.ModelChanged(Self);
end;

procedure TFCHARBulkEdit.ShowEditor(const M3: TM3File);
var
  i, idx: Integer;
  s: string;
begin
  FInitCharTags.Clear;
  for i := 0 to M3.TagCount-1 do
  with m3[i]^ do
    if (StructName = 'CHAR') and (SpecialType <> sstCharBinary) then
    begin
      s := PChar(Data);
      FInitCharTags.Add(IntToStr(Index)+':'+s);
    end;
  BResetClick(nil);
  if ShowModal = mrOK then
  begin
    MemoCHAR.Lines.NameValueSeparator := ':';
    for i := 0 to MemoCHAR.Lines.Count-1 do
    begin
      idx := StrToIntDef(MemoCHAR.Lines.Names[i],-1);
      if (idx >= 0) and (idx < m3.TagCount) then
        EditCHAR(m3[idx]^,m3,MemoCHAR.Lines.ValueFromIndex[i])
      else
        FMain.Log('CHAR bulk edit: index "%s" on line %d out of bounds',[MemoCHAR.Lines.Names[i],i+1]);
    end;
  end;
end;

end.

