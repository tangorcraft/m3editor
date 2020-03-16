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
unit uEditFloat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFEditFloat }

  TFEditFloat = class(TForm)
    BCancel: TButton;
    BOk: TButton;
    BReset: TButton;
    Edit: TEdit;
    lblExp: TLabel;
    lblValue: TLabel;
    procedure BResetClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private

  public
    FInitValue: Single;
  end;

var
  FEditFloat: TFEditFloat;

implementation

uses
  uCommon;

{$R *.lfm}

{ TFEditFloat }

procedure TFEditFloat.BResetClick(Sender: TObject);
begin
  Edit.Text := FloatToStr(FInitValue,FloatDotFormat);
end;

procedure TFEditFloat.EditChange(Sender: TObject);
var
  f: single;
begin
  f := StrToFloatDef(Edit.Text,0,FloatDotFormat);
  lblValue.Caption := Format('Value: %s',[FloatToStr(f,FloatDotFormat)]);
  lblExp.Caption := Format('Scientific: %e',[f]);
end;

end.

