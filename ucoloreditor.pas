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
unit uColorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Spin, ustructures, uM3File;

type

  { TFEditColor }

  TFEditColor = class(TForm)
    BCancel: TButton;
    BOk: TButton;
    BReset: TButton;
    ColorDialog: TColorDialog;
    editRed: TFloatSpinEdit;
    editGreen: TFloatSpinEdit;
    editBlue: TFloatSpinEdit;
    editHue: TFloatSpinEdit;
    editSat: TFloatSpinEdit;
    editLum: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ShapeOld: TShape;
    ShapeNew: TShape;
    TrackBarRed: TTrackBar;
    TrackBarGreen: TTrackBar;
    TrackBarBlue: TTrackBar;
    TrackBarHue: TTrackBar;
    TrackBarSat: TTrackBar;
    TrackBarLum: TTrackBar;
    procedure BResetClick(Sender: TObject);
    procedure editBlueChange(Sender: TObject);
    procedure editGreenChange(Sender: TObject);
    procedure editHueChange(Sender: TObject);
    procedure editLumChange(Sender: TObject);
    procedure editRedChange(Sender: TObject);
    procedure editSatChange(Sender: TObject);
    procedure TrackBarBlueChange(Sender: TObject);
    procedure TrackBarGreenChange(Sender: TObject);
    procedure TrackBarHueChange(Sender: TObject);
    procedure TrackBarLumChange(Sender: TObject);
    procedure TrackBarRedChange(Sender: TObject);
    procedure TrackBarSatChange(Sender: TObject);
  private
    FInitVec: m3VEC3_color;
    FCurVec: m3VEC3_color;

    FHue: Single;
    FSat: Single;
    FLum: Single;

    FDisplaying: Boolean;

    procedure DisplayShapes;
    procedure DisplayRGB;
    procedure DisplayHSL;

    procedure CalcRGB;
    procedure CalcHSL;

    function MakeCOL(const alpha: Byte): m3Color;
  public
    function ShowEditorVEC3(const VEC: Pm3VEC3_color): boolean;
    function ShowEditorCOL(const COL: Pm3Color): boolean;
  end;

var
  FEditColor: TFEditColor;

implementation

uses
  uCommon;

{$R *.lfm}

{ TFEditColor }

procedure TFEditColor.BResetClick(Sender: TObject);
begin
  FCurVec := FInitVec;
  DisplayShapes;
  DisplayRGB;
  CalcHSL;
end;

procedure TFEditColor.editBlueChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  FCurVec.B := editBlue.Value / 255.0;
  TrackBarBlue.Position := round(editBlue.Value*100);
  DisplayShapes;
  CalcHSL;
end;

procedure TFEditColor.editGreenChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  FCurVec.G := editGreen.Value / 255.0;
  TrackBarGreen.Position := round(editGreen.Value*100);
  DisplayShapes;
  CalcHSL;
end;

procedure TFEditColor.editHueChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  FHue := editHue.Value;
  TrackBarHue.Position := round(editHue.Value*100);
  CalcRGB;
end;

procedure TFEditColor.editLumChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  FLum := editLum.Value / 100.0;
  TrackBarLum.Position := round(editLum.Value*100);
  CalcRGB;
end;

procedure TFEditColor.editRedChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  FCurVec.R := editRed.Value / 255.0;
  TrackBarRed.Position := round(editRed.Value*100);
  DisplayShapes;
  CalcHSL;
end;

procedure TFEditColor.editSatChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  FSat := editSat.Value / 100.0;
  TrackBarSat.Position := round(editSat.Value*100);
  CalcRGB;
end;

procedure TFEditColor.TrackBarBlueChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  editBlue.Value := TrackBarBlue.Position / 100.0;
  FCurVec.B := editBlue.Value / 255.0;
  DisplayShapes;
  CalcHSL;
end;

procedure TFEditColor.TrackBarGreenChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  editGreen.Value := TrackBarGreen.Position / 100.0;
  FCurVec.G := editGreen.Value / 255.0;
  DisplayShapes;
  CalcHSL;
end;

procedure TFEditColor.TrackBarHueChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  editHue.Value := TrackBarHue.Position / 100.0;
  FHue := editHue.Value;
  CalcRGB;
end;

procedure TFEditColor.TrackBarLumChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  editLum.Value := TrackBarLum.Position / 100.0;
  FLum := editLum.Value / 100.0;
  CalcRGB;
end;

procedure TFEditColor.TrackBarRedChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  editRed.Value := TrackBarRed.Position / 100.0;
  FCurVec.R := editRed.Value / 255.0;
  DisplayShapes;
  CalcHSL;
end;

procedure TFEditColor.TrackBarSatChange(Sender: TObject);
begin
  if FDisplaying then Exit;
  editSat.Value := TrackBarSat.Position / 100.0;
  FSat := editSat.Value / 100.0;
  CalcRGB;
end;

procedure TFEditColor.DisplayShapes;
begin
  ShapeOld.Brush.Color := VEC3ToColor(FInitVec);
  ShapeNew.Brush.Color := VEC3ToColor(FCurVec);
end;

procedure TFEditColor.DisplayRGB;
begin
  FDisplaying := True;
  editRed.Value := FCurVec.R * 255.0;
  editGreen.Value := FCurVec.G * 255.0;
  editBlue.Value := FCurVec.B * 255.0;

  TrackBarRed.Position := round(editRed.Value*100);
  TrackBarGreen.Position := round(editGreen.Value*100);
  TrackBarBlue.Position := round(editBlue.Value*100);
  FDisplaying := False;
end;

procedure TFEditColor.DisplayHSL;
begin
  FDisplaying := True;
  editHue.Value := FHue;
  editSat.Value := FSat * 100;
  editLum.Value := FLum * 100;

  TrackBarHue.Position := round(editHue.Value*100);
  TrackBarSat.Position := round(editSat.Value*100);
  TrackBarLum.Position := round(editLum.Value*100);
  FDisplaying := False;
end;

procedure TFEditColor.CalcRGB;
var
  hue: Single;
  tmp1, tmp2: Single;
  tmpVec: m3VEC3_color;
const
  oneOf360 = 1.0 / 360.0;
  oneOf6 = 1.0 / 6.0;
  twoOf3 = 2.0 / 3.0;

  function CalcHSLToRGBColor(const Col: Single): Single;
  begin
    if Col < oneOf6 then Result := tmp2 + ((tmp1 - tmp2) * 6 * Col)
    else if Col < 0.5 then Result := tmp1
    else if Col < twoOf3 then Result := tmp2 + ((tmp1 - tmp2) * 6 * (twoOf3 - Col))
    else Result := tmp2;
  end;

begin
  if FSat = 0 then
  begin
    FCurVec.R := FLum;
    FCurVec.G := FLum;
    FCurVec.B := FLum;
    DisplayShapes;
    DisplayRGB;
    Exit;
  end;
  if FLum < 0.5 then
    tmp1 := FLum * (1.0 + FSat)
  else
    tmp1 := FLum + FSat - (FLum * FSat);
  tmp2 := 2 * FLum - tmp1;

  with tmpVec do
  begin
    R := FHue + 120.0;
    if R > 360 then R := R - 360.0;
    B := (FHue - 120.0);
    if B < 0 then B := B + 360.0;

    R := R * oneOf360;
    G := FHue * oneOf360;
    B := B * oneOf360;
  end;
  hue := tmpVec.B;

  FCurVec.R := CalcHSLToRGBColor(tmpVec.R);
  FCurVec.G := CalcHSLToRGBColor(tmpVec.G);
  FCurVec.B := CalcHSLToRGBColor(tmpVec.B);
  DisplayShapes;
  DisplayRGB;
end;

procedure TFEditColor.CalcHSL;
var
  min, max, diff, sum: Single;
begin
  min := 1;
  max := 0;
  with FCurVec do
  begin
    if R < min then min := R;
    if G < min then min := G;
    if B < min then min := B;

    if R > max then max := R;
    if G > max then max := G;
    if B > max then max := B;
  end;

  FLum := (min + max)/2.0;

  diff := max - min;
  if (diff = 0) then
  begin
    FHue := 0;
    FSat := 0;
  end
  else
  begin
    sum := max + min;
    if FLum <= 0.5 then
      FSat := diff / sum
    else
      FSat := diff / (2.0 - sum);

    with FCurVec do
    begin
      if R = max then FHue := (G - B) / diff
      else if G = max then FHue := 2.0 + (B - R) / diff
      else {if B = max then} FHue := 4.0 + (R - G) / diff;
    end;
    FHue := FHue * 60;
    while FHue > 360.0 do FHue := FHue - 360.0;
    while FHue < 0 do FHue := FHue + 360.0;
  end;
  DisplayHSL;
end;

function TFEditColor.MakeCOL(const alpha: Byte): m3Color;
begin
  Result.blue := round(FCurVec.B * 255);
  Result.green := round(FCurVec.G * 255);
  Result.red := round(FCurVec.R * 255);
  Result.alpha := alpha;
end;

function TFEditColor.ShowEditorVEC3(const VEC: Pm3VEC3_color): boolean;
begin
  FInitVec := VEC^;
  BResetClick(nil);

  Result := (ShowModal = mrOK);
  if Result then
  begin
    VEC^ := FCurVec;
  end;
end;

function TFEditColor.ShowEditorCOL(const COL: Pm3Color): boolean;
begin
  FInitVec.R := COL^.red / 255.0;
  FInitVec.G := COL^.green / 255.0;
  FInitVec.B := COL^.blue / 255.0;
  BResetClick(nil);

  Result := (ShowModal = mrOK);
  if Result then
  begin
    COL^ := MakeCOL(COL^.alpha);
  end;
end;

end.

