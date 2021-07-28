(*
    This file is a part of "M3 Editor" project <https://github.com/tangorcraft/m3editor/>.
    Copyright (C) 2020-2021  Ivan Markov (TangorCraft)

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
unit uToolTextureRename;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  uM3File, ustructures;

type

  { TFTextureRename }

  TFTextureRename = class(TForm)
    BResetPath: TButton;
    BResetChannel: TButton;
    BSave: TButton;
    BCancel: TButton;
    BResetAll: TButton;
    cbChannel: TComboBox;
    EditTexture: TEdit;
    gridTextures: TStringGrid;
    Label1: TLabel;
    Label2: TLabel;
    procedure BResetAllClick(Sender: TObject);
    procedure BResetChannelClick(Sender: TObject);
    procedure BResetPathClick(Sender: TObject);
    procedure cbChannelChange(Sender: TObject);
    procedure EditTextureChange(Sender: TObject);
    procedure gridTexturesClick(Sender: TObject);
  private
    FOldNames: array of string;
    FNewNames: array of string;
    FOldChannel: array of Integer;
    FNewChannel: array of Integer;
    FItemCount: Integer;

    FUpdating: Boolean;

    function TexturePathAdd(const S:String; const Ch: Integer): integer;
    function TextureChannelGet(const M3: TM3File; const Struct: TM3Structure): Integer;

    procedure UpdateCell(const Idx: Integer);
    procedure ProcessEditTag(const M3: TM3File; var Struct: TM3Structure);
  public
    procedure ShowEditor(const M3: TM3File);
  end;

var
  FTextureRename: TFTextureRename;

implementation

uses
  umain, uCommon;

{$R *.lfm}

{
Allowed colorChannelSetting values:
                0: RGB
                1: RGBA (used for alpha)
                2: Alpha Only
                3: Red Only
                4: Green Only
                5: Blue Only
}
const
  channelRGB  = 0;
  channelRGBA = 1;
  channelA    = 2;
  channelR    = 3;
  channelG    = 4;
  channelB    = 5;
  channelVarious = 6;

function ChannelStr(const Ch: Integer): String;
begin
  case Ch of
    channelRGB: Result := 'RGB';
    channelRGBA: Result := 'RGBA';
    channelA: Result := 'Alpha Only';
    channelR: Result := 'Red Only';
    channelG: Result := 'Green Only';
    channelB: Result := 'Blue Only';
  else Result := 'Various';
  end;
end;

function ReplaceSlashes(const S: string): string;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to length(Result) do
    if Result[i]='\' then Result[i] := '/';
end;

{ TFTextureRename }

procedure TFTextureRename.gridTexturesClick(Sender: TObject);
begin
  FUpdating := true;
  EditTexture.Text := FNewNames[gridTextures.Row-1];
  cbChannel.ItemIndex := FNewChannel[gridTextures.Row-1];
  FUpdating := false;
end;

procedure TFTextureRename.EditTextureChange(Sender: TObject);
begin
  if FUpdating then exit;
  FNewNames[gridTextures.Row-1] := ReplaceSlashes(EditTexture.Text);
  UpdateCell(gridTextures.Row-1);
end;

procedure TFTextureRename.cbChannelChange(Sender: TObject);
begin
  if FUpdating then exit;
  FNewChannel[gridTextures.Row-1] := cbChannel.ItemIndex;
  UpdateCell(gridTextures.Row-1);
end;

procedure TFTextureRename.BResetPathClick(Sender: TObject);
begin
  FUpdating := true;
  FNewNames[gridTextures.Row-1] := FOldNames[gridTextures.Row-1];
  EditTexture.Text := FNewNames[gridTextures.Row-1];
  FUpdating := false;
end;

procedure TFTextureRename.BResetChannelClick(Sender: TObject);
begin
  FUpdating := true;
  FNewChannel[gridTextures.Row-1] := FOldChannel[gridTextures.Row-1];
  cbChannel.ItemIndex := FNewChannel[gridTextures.Row-1];
  FUpdating := false;
end;

procedure TFTextureRename.BResetAllClick(Sender: TObject);
var
  i: Integer;
begin
  FUpdating := true;
  for i := 0 to FItemCount-1 do
  begin
    FNewNames[i] := FOldNames[i];
    FNewChannel[i] := FOldChannel[i];
    UpdateCell(i);
  end;
  EditTexture.Text := FNewNames[gridTextures.Row-1];
  cbChannel.ItemIndex := FNewChannel[gridTextures.Row-1];
  FUpdating := false;
end;

function TFTextureRename.TexturePathAdd(const S: String;
  const Ch: Integer): integer;
var
  i: Integer;
  ext: String;
begin
  Result := -1;
  //if Pos('.',S)=0 then exit;
  ext := LowerCase(ExtractFileExt(S));
  if (ext <> '.dds')and(ext <> '.tga') then Exit;
  for i := 0 to FItemCount-1 do
    if S = FOldNames[i] then
    begin
      if FOldChannel[i] = -1 then
      begin
        FOldChannel[i] := Ch;
        FNewChannel[i] := Ch;
      end
      else if FOldChannel[i] <> Ch then
      begin
        FOldChannel[i] := channelVarious;
        FNewChannel[i] := channelVarious;
      end;
      Exit(i);
    end;
  Result := FItemCount;
  SetLength(FOldNames,FItemCount+1);
  SetLength(FNewNames,FItemCount+1);
  SetLength(FOldChannel,FItemCount+1);
  SetLength(FNewChannel,FItemCount+1);
  FOldNames[FItemCount] := S;
  FNewNames[FItemCount] := S;
  FOldChannel[FItemCount] := Ch;
  FNewChannel[FItemCount] := Ch;
  inc(FItemCount);
end;

function TFTextureRename.TextureChannelGet(const M3: TM3File;
  const Struct: TM3Structure): Integer;
var
  i: Integer;
  tmp: Integer;
begin
  Result := -1;
  for i := 0 to length(Struct.RefFrom)-1 do
    if m3[Struct.RefFrom[i].rfTagIndex]^.Tag = LAYRTag then
    with m3[Struct.RefFrom[i].rfTagIndex]^ do
    begin
      if ReadFieldData(m3[Struct.RefFrom[i].rfTagIndex]^,'colorChannelSetting',Struct.RefFrom[i].rfItemIndex,tmp,4) then
      begin
        if (Result = -1)or(Result = tmp) then
          Result := tmp
        else
          Result := channelVarious;
      end;
    end;
end;

procedure TFTextureRename.UpdateCell(const Idx: Integer);
begin
  if (Idx >= 0)and(Idx < FItemCount) then
  begin
    gridTextures.Cells[0,Idx+1] := Format('%s [%s]',[FOldNames[Idx],ChannelStr(FOldChannel[Idx])]);
    gridTextures.Cells[1,Idx+1] := Format('%s [%s]',[FNewNames[Idx],ChannelStr(FNewChannel[Idx])]);
  end;
end;

procedure TFTextureRename.ProcessEditTag(const M3: TM3File;
  var Struct: TM3Structure);
var
  s: String;
  idx, i: Integer;
begin
  s := PChar(Struct.Data);
  for idx := 0 to FItemCount-1 do
  if FOldNames[idx] = s then
  begin
    if FOldNames[idx] <> FNewNames[idx] then
    begin
      M3.EditCHARTag(Struct,FNewNames[idx]);
      FMain.Log('Edit %d:CHAR = "%s"',[Struct.Index,FNewNames[idx]]);
    end;
    if (FOldChannel[idx] <> FNewChannel[idx]) and (FNewChannel[idx] <> channelVarious) then
    begin
      for i := 0 to length(Struct.RefFrom)-1 do
        if m3[Struct.RefFrom[i].rfTagIndex]^.Tag = LAYRTag then
        with m3[Struct.RefFrom[i].rfTagIndex]^ do
        begin
          if WriteFieldData(m3[Struct.RefFrom[i].rfTagIndex]^,'colorChannelSetting',Struct.RefFrom[i].rfItemIndex,FNewChannel[idx],4) then
          begin
            FMain.Log('Edit %d:LAYR [%d] colorChannelSetting = %s',[
              Struct.RefFrom[i].rfTagIndex,
              Struct.RefFrom[i].rfItemIndex,
              ChannelStr(FNewChannel[idx])
            ]);
          end
          else
          begin
            FMain.Log('ERROR! Can''t edit %d:LAYR [%d] colorChannelSetting',[
              Struct.RefFrom[i].rfTagIndex,
              Struct.RefFrom[i].rfItemIndex
            ]);
          end;
        end;
    end;
  end;
end;

procedure TFTextureRename.ShowEditor(const M3: TM3File);
var
  i: Integer;
begin
  FItemCount := 0;
  for i := 0 to M3.TagCount-1 do with m3[i]^ do
    if (Tag = CHARTag) and (SpecialType <> sstCharBinary) then
      TexturePathAdd(PChar(Data), TextureChannelGet(m3,m3[i]^));

  if FItemCount = 0 then
  begin
    MessageDlg('No textures','No CHAR tags containing path to textures found.',mtInformation,[mbOK],0);
    Exit;
  end;

  gridTextures.RowCount := FItemCount+1;
  for i := 0 to FItemCount-1 do
  begin
    UpdateCell(i);
  end;

  gridTextures.Row := 1;
  gridTexturesClick(nil);
  if ShowModal = mrOK then
  begin
    for i := 0 to M3.TagCount-1 do with m3[i]^ do
      if (Tag = CHARTag) and (SpecialType <> sstCharBinary) then
        ProcessEditTag(m3,m3[i]^);
    FMain.ModelChanged(nil);
  end;
  SetLength(FOldNames,0);
  SetLength(FNewNames,0);
  SetLength(FOldChannel,0);
  SetLength(FNewChannel,0);
end;

end.

