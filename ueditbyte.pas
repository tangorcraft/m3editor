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
unit uEditByte;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFEditByte }

  TFEditByte = class(TForm)
    BCancel: TButton;
    BFlags: TButton;
    BOk: TButton;
    BReset: TButton;
    EditHex: TEdit;
    EditSigned: TEdit;
    EditUnSigned: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblFixed: TLabel;
    procedure BResetClick(Sender: TObject);
    procedure EditHexChange(Sender: TObject);
    procedure EditSignedChange(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditUnSignedChange(Sender: TObject);
  private
    FUpdating: Boolean;
    procedure UpdateEdits(const Exclude: TEdit);
  public
    FCurValue: UInt8;
    FInitValue: UInt8;
  end;

var
  FEditByte: TFEditByte;

implementation

uses
  uCommon;

{$R *.lfm}

{ TFEditByte }

procedure TFEditByte.BResetClick(Sender: TObject);
begin
  FCurValue := FInitValue;
  UpdateEdits(nil);
end;

procedure TFEditByte.EditHexChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FCurValue := StrToIntDef('0x'+EditHex.Text,0);
  UpdateEdits(EditHex);
end;

procedure TFEditByte.EditSignedChange(Sender: TObject);
var
  i: Int8;
begin
  if FUpdating then Exit;
  i := StrToIntDef(EditSigned.Text,0);
  FCurValue := i;
  UpdateEdits(EditSigned);
end;

procedure TFEditByte.EditExit(Sender: TObject);
begin
  UpdateEdits(nil);
end;

procedure TFEditByte.EditUnSignedChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FCurValue := StrToIntDef(EditUnSigned.Text,0);
  UpdateEdits(EditUnSigned);
end;

procedure TFEditByte.UpdateEdits(const Exclude: TEdit);
begin
  FUpdating := true;
  if EditSigned<>Exclude then
    EditSigned.Text := IntToStr(Int8(FCurValue));
  if EditUnSigned<>Exclude then
    EditUnSigned.Text := IntToStr(FCurValue);
  if EditHex<>Exclude then
    EditHex.Text := IntToHex(FCurValue,2);
  lblFixed.Caption := Format('Real: %s',[FloatToStrM3((FCurValue / 255.0)*2 - 1)]);
  FUpdating := false;
end;

end.

