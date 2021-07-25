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
unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFAbout }

  TFAbout = class(TForm)
    BOK: TButton;
    BSource: TButton;
    BLicense: TButton;
    BLazarus: TButton;
    ImageLaz: TImage;
    Label1: TLabel;
    procedure BLazarusClick(Sender: TObject);
    procedure BLicenseClick(Sender: TObject);
    procedure BSourceClick(Sender: TObject);
  private

  public

  end;

var
  FAbout: TFAbout;

implementation

uses
  LCLIntf;

{$R *.lfm}

{ TFAbout }

procedure TFAbout.BSourceClick(Sender: TObject);
begin
  OpenURL('https://github.com/tangorcraft/m3editor');
end;

procedure TFAbout.BLicenseClick(Sender: TObject);
begin
  OpenURL('https://www.gnu.org/licenses/');
end;

procedure TFAbout.BLazarusClick(Sender: TObject);
begin
  OpenURL('https://www.lazarus-ide.org/');
end;

end.

