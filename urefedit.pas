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
unit uRefEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uM3File, ustructures;

type

  { TFRefEdit }

  TFRefEdit = class(TForm)
    BCancel: TButton;
    BOk: TButton;
    BReset: TButton;
    BAsFlags: TButton;
    comboTags: TComboBox;
    EditCount: TEdit;
    EditFlags: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BAsFlagsClick(Sender: TObject);
    procedure BResetClick(Sender: TObject);
    procedure comboTagsSelect(Sender: TObject);
    procedure EditCountExit(Sender: TObject);
    procedure EditFlagsExit(Sender: TObject);
  private
    FFiledTagIdx: Integer;
    FRef: Pm3ref;
    FM3File: TM3File;
  public
    function ShowEditor(const M3: TM3File; const F: TM3Field; const idx: Integer): boolean;
  end;

var
  FRefEdit: TFRefEdit;

implementation

uses
  uCommon, uEditFlags;

{$R *.lfm}

{ TFRefEdit }

procedure TFRefEdit.BResetClick(Sender: TObject);
begin
  comboTags.ItemIndex := FRef^.refIndex;
  EditCount.Text := IntToStr(FRef^.refCount);
  if EditFlags.Enabled then
    EditFlags.Text := IntToHex(FRef^.refFlags,8);
end;

procedure TFRefEdit.BAsFlagsClick(Sender: TObject);
begin
  with TFEditFlags.Create(Self) do
  try
    FInitValue := StrToIntDef('0x'+EditFlags.Text,0);
    BResetClick(nil);
    if ShowModal = mrOK then
      EditFlags.Text := IntToHex(FInitValue,8);
  finally
    Free;
  end;
end;

procedure TFRefEdit.comboTagsSelect(Sender: TObject);
begin
  BOk.Enabled := False;
  if comboTags.ItemIndex = 0 then
  begin
    BOk.Enabled := True;
    EditCount.Text := '0';
    EditFlags.Text := '0';
  end
  else if comboTags.ItemIndex > 0 then
  begin
    BOk.Enabled := (comboTags.ItemIndex <> FFiledTagIdx);
    EditCount.Text := IntToStr(FM3File[comboTags.ItemIndex]^.ItemCount);
  end;
end;

procedure TFRefEdit.EditCountExit(Sender: TObject);
begin
  EditCount.Text := IntToStr(StrToIntDef(EditCount.Text,0));
end;

procedure TFRefEdit.EditFlagsExit(Sender: TObject);
begin
  EditFlags.Text := IntToHex(StrToIntDef('0x'+EditFlags.Text,0),8);
end;

function TFRefEdit.ShowEditor(const M3: TM3File; const F: TM3Field;
  const idx: Integer): boolean;
var
  i: Integer;
begin
  FFiledTagIdx := idx;
  FRef := F.fData;
  FM3File := M3;

  comboTags.Items.Clear;
  comboTags.Items.Add('0: No Reference');
  for i := 1 to M3.TagCount-1 do
    comboTags.Items.Add(GetTreeTagName(m3[i]^));
  if FRef^.refIndex < M3.TagCount then
    comboTags.ItemIndex := FRef^.refIndex
  else
    comboTags.ItemIndex := 0;
  EditCount.Text := IntToStr(FRef^.refCount);
  if F.fType = ftRefSmall then
  begin
    EditFlags.Text := '0';
    EditFlags.Enabled := false;
    BAsFlags.Enabled := false;
  end
  else
    EditFlags.Text := IntToHex(FRef^.refFlags,8);

  Result := (ShowModal = mrOK);
  if Result then
  begin
    FRef^.refIndex := comboTags.ItemIndex;
    FRef^.refCount := StrToIntDef(EditCount.Text,0);
    if (FRef^.refIndex = 0) or (FRef^.refCount = 0) then
    begin
      FRef^.refIndex := 0;
      FRef^.refCount := 0;
      if F.fType = ftRef then
        FRef^.refFlags := 0;
    end
    else if F.fType = ftRef then
      FRef^.refFlags := StrToIntDef('0x'+EditFlags.Text,0);
  end;
end;

end.

