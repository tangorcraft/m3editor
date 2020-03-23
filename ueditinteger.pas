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
unit uEditInteger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFEditInteger }

  TFEditInteger = class(TForm)
    BCancel: TButton;
    BOk: TButton;
    BReset: TButton;
    BFlags: TButton;
    EditSigned: TEdit;
    EditUnSigned: TEdit;
    EditHex: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BResetClick(Sender: TObject);
    procedure EditHexChange(Sender: TObject);
    procedure EditSignedChange(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditUnSignedChange(Sender: TObject);
  private
    FUpdating: boolean;
    procedure UpdateEdits(const Exclude: TEdit);
  public
    FCurValue: UInt32;
    FInitValue: UInt32;
  end;

var
  FEditInteger: TFEditInteger;

implementation

{$R *.lfm}

{ TFEditInteger }

procedure TFEditInteger.BResetClick(Sender: TObject);
begin
  FCurValue := FInitValue;
  UpdateEdits(nil);
end;

procedure TFEditInteger.EditHexChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FCurValue := StrToIntDef('0x'+EditHex.Text,0);
  UpdateEdits(EditHex);
end;

procedure TFEditInteger.EditSignedChange(Sender: TObject);
var
  i: Int32;
begin
  if FUpdating then Exit;
  i := StrToIntDef(EditSigned.Text,0);
  FCurValue := i;
  UpdateEdits(EditSigned);
end;

procedure TFEditInteger.EditExit(Sender: TObject);
begin
  UpdateEdits(nil);
end;

procedure TFEditInteger.EditUnSignedChange(Sender: TObject);
begin
  if FUpdating then Exit;
  FCurValue := StrToIntDef(EditUnSigned.Text,0);
  UpdateEdits(EditUnSigned);
end;

procedure TFEditInteger.UpdateEdits(const Exclude: TEdit);
begin
  FUpdating := true;
  if EditSigned<>Exclude then
    EditSigned.Text := IntToStr(Int32(FCurValue));
  if EditUnSigned<>Exclude then
    EditUnSigned.Text := IntToStr(FCurValue);
  if EditHex<>Exclude then
    EditHex.Text := IntToHex(FCurValue,8);
  FUpdating := false;
end;

end.

