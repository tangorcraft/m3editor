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
unit uEditFlags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFEditFlags }

  TFEditFlags = class(TForm)
    BCancel: TButton;
    BOk: TButton;
    BReset: TButton;
    cbValues: TCheckGroup;
    lblVal: TLabel;
    procedure BResetClick(Sender: TObject);
    procedure cbValuesItemClick(Sender: TObject; Index: integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    procedure DisplayVal;
  public
    FInitValue: UInt32;
    FVal8: UInt8;
    FVal16: UInt16;
  end;

var
  FEditFlags: TFEditFlags;

implementation

{$R *.lfm}

{ TFEditFlags }

procedure TFEditFlags.BResetClick(Sender: TObject);
var
  i: integer;
  v: UInt32;
begin
  v := 1;
  for i := 0 to 31 do
  begin
    cbValues.Checked[i] := ((FInitValue and v)<>0);
    v := v shl 1;
  end;
  DisplayVal;
end;

procedure TFEditFlags.cbValuesItemClick(Sender: TObject; Index: integer);
begin
  DisplayVal;
end;

procedure TFEditFlags.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
  v: UInt32;
begin
  FInitValue := 0;
  v := 1;
  for i := 0 to 31 do
  begin
    if cbValues.Checked[i] then
      FInitValue := FInitValue or v;
    v := v shl 1;
  end;
  FVal8 := FInitValue and $FF;
  FVal16 := FInitValue and $FFFF;
end;

procedure TFEditFlags.DisplayVal;
var
  i: integer;
  f, v: UInt32;
begin
  f := 0;
  v := 1;
  for i := 0 to 31 do
  begin
    if cbValues.Checked[i] then
      f := f or v;
    v := v shl 1;
  end;
  lblVal.Caption := Format('Value: 0x%.8x',[f]);
end;

end.

