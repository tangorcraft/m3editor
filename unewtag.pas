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
unit uNewTag;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ustructures, uM3File;

type

  { TFNewTag }

  TFNewTag = class(TForm)
    BOk: TButton;
    BCancel: TButton;
    comboTags: TComboBox;
    lblIndex: TLabel;
    MemoDesc: TMemo;
    btnPlus: TSpeedButton;
    btnMinus: TSpeedButton;
    procedure btnMinusClick(Sender: TObject);
    procedure btnPlusClick(Sender: TObject);
    procedure comboTagsSelect(Sender: TObject);
  private
    FIdx: Integer;
    FM3File: TM3File;
  public
    function ShowAddTag(const M3: TM3File; const newIdx: Integer): Boolean;
    function ShowEditTag(const M3: TM3File; const newIdx: Integer): Boolean;
  end;

var
  FNewTag: TFNewTag;

implementation

uses
  uCommon;

{$R *.lfm}

{ TFNewTag }

procedure TFNewTag.comboTagsSelect(Sender: TObject);
begin
  if (comboTags.ItemIndex >= 0) and (comboTags.ItemIndex < Structures.TagInfoCount) then
    MemoDesc.Text := Structures.GetStructureDescFromName(Structures.TagInfo[comboTags.ItemIndex].Name);
end;

procedure TFNewTag.btnPlusClick(Sender: TObject);
begin
  if FIdx < (FM3File.TagCount) then
  begin
    Inc(FIdx);
    lblIndex.Caption := 'Add new tag at index: '+IntToStr(FIdx);
  end;
end;

procedure TFNewTag.btnMinusClick(Sender: TObject);
begin
  if FIdx > 0 then
  begin
    Dec(FIdx);
    lblIndex.Caption := 'Add new tag at index: '+IntToStr(FIdx);
  end;
end;

function TFNewTag.ShowAddTag(const M3: TM3File; const newIdx: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Structures.TagInfoCount-1 do
    comboTags.Items.Add(Structures.TagInfo[i].DisplayName);
  FIdx := newIdx;
  FM3File := M3;
  lblIndex.Caption := 'Add new tag at index: '+IntToStr(FIdx);
  comboTags.ItemIndex := 0;

  Result := (ShowModal = mrOK) and (comboTags.ItemIndex >= 0) and (comboTags.ItemIndex < Structures.TagInfoCount);
  if Result then
  begin
    if FIdx >= M3.TagCount then
      FIdx := M3.AppendEmptyTag
    else
      M3.InsertEmptyTag(FIdx);
    with Structures.TagInfo[comboTags.ItemIndex] do
    begin
      M3[FIdx]^.Tag := Tag;
      M3[FIdx]^.StructName := Name;
      M3[FIdx]^.Ver := Ver;
      M3[FIdx]^.ItemSize := Size;
    end;
    Structures.GetStructureInfo(M3[FIdx]^);
    DuplicateStructureItem(M3[FIdx]^,0);
    if M3[FIdx]^.Tag = headerTag33 then
      Pm3Header(M3[FIdx]^.Data)^.tag := headerTag33;
    if M3[FIdx]^.Tag = headerTag34 then
      Pm3Header(M3[FIdx]^.Data)^.tag := headerTag34;
  end;
end;

function TFNewTag.ShowEditTag(const M3: TM3File;
  const newIdx: Integer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Structures.TagInfoCount-1 do
    comboTags.Items.Add(Structures.TagInfo[i].DisplayName);
  FIdx := newIdx;
  FM3File := M3;
  btnPlus.Visible := false;
  btnMinus.Visible := false;
  lblIndex.Caption := 'Edit tag at index: '+IntToStr(FIdx);
  Caption := 'Change tag ID';
  for i := 0 to Structures.TagInfoCount-1 do
    if (Structures.TagInfo[i].Tag = M3[FIdx]^.Tag) and (Structures.TagInfo[i].Ver = M3[FIdx]^.Ver) then
    begin
      comboTags.ItemIndex := i;
      Break;
    end;

  Result := (ShowModal = mrOK) and (comboTags.ItemIndex >= 0) and (comboTags.ItemIndex < Structures.TagInfoCount);
  if Result then
  begin
    M3.ResetTag(FIdx);
    with Structures.TagInfo[comboTags.ItemIndex] do
    begin
      M3[FIdx]^.Tag := Tag;
      M3[FIdx]^.StructName := Name;
      M3[FIdx]^.Ver := Ver;
      M3[FIdx]^.ItemSize := Size;
    end;
    Structures.GetStructureInfo(M3[FIdx]^);
    DuplicateStructureItem(M3[FIdx]^,0);
    if M3[FIdx]^.Tag = headerTag33 then
      Pm3Header(M3[FIdx]^.Data)^.tag := headerTag33;
    if M3[FIdx]^.Tag = headerTag34 then
      Pm3Header(M3[FIdx]^.Data)^.tag := headerTag34;
  end;
end;

end.

