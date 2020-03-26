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
unit u3DViewForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, uM3File, ustructures;

type

  { TF3dView }

  TF3dView = class(TForm)
    Panel3D: TPanel;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    PanelBottom: TPanel;
  private
    FM3File: TM3File;
    FVertexFlags: UInt32;
    FVertexArray: array of TM3VertexInfoFull;

    procedure ReadVertexArray(vData: Pointer; DataSize: Integer);
  public
    procedure ShowEditor(const M3: TM3File);
  end;

var
  F3dView: TF3dView;

implementation

uses
  uCommon;

const
  vertexSizeMin =
    12+ // VEC3, 3 singles
    4+ // BoneWeight: 1 byte each * 4
    4+ // BoneLookupIndex: 1 byte each * 4
    4+ // Normal, 4 bytes
    4+ // UV0, 2 words
    4 // Tangent, 4 bytes
  ;
  vFlag_Color_bit = $200;
  vFlag_hasVertices_bit = $20000;
  vFlag_UV1_bit = $40000;
  vFlag_UV2_bit = $80000;
  vFlag_UV3_bit = $100000;

{$R *.lfm}

{ TF3dView }

procedure TF3dView.ReadVertexArray(vData: Pointer; DataSize: Integer);
var
  i, vSize: Integer;
  hasColor, hasUV1, hasUV2, hasUV3: boolean;
begin
  SetLength(FVertexArray,0);
  if (FVertexFlags and vFlag_hasVertices_bit)=0 then Exit;
  vSize := vertexSizeMin;
  hasColor := ((FVertexFlags and vFlag_Color_bit)<>0);
  if hasColor then inc(vSize,4);
  hasUV1 := ((FVertexFlags and vFlag_UV1_bit)<>0);
  if hasUV1 then inc(vSize,4);
  hasUV2 := ((FVertexFlags and vFlag_UV2_bit)<>0);
  if hasUV2 then inc(vSize,4);
  hasUV3 := ((FVertexFlags and vFlag_UV3_bit)<>0);
  if hasUV3 then inc(vSize,4);
  i := DataSize div vSize;
  SetLength(FVertexArray,i);
  for i := 0 to length(FVertexArray)-1 do
  begin
    FVertexArray[i].position := Pm3VEC3(vData)^;
    inc(vData,12); //sizeof(m3VEC3)

    FVertexArray[i].boneWeight0 := PUInt8(vData)^;
    inc(vData);
    FVertexArray[i].boneWeight1 := PUInt8(vData)^;
    inc(vData);
    FVertexArray[i].boneWeight2 := PUInt8(vData)^;
    inc(vData);
    FVertexArray[i].boneWeight3 := PUInt8(vData)^;
    inc(vData);

    FVertexArray[i].boneLookupIndex0 := PUInt8(vData)^;
    inc(vData);
    FVertexArray[i].boneLookupIndex1 := PUInt8(vData)^;
    inc(vData);
    FVertexArray[i].boneLookupIndex2 := PUInt8(vData)^;
    inc(vData);
    FVertexArray[i].boneLookupIndex3 := PUInt8(vData)^;
    inc(vData);

    FVertexArray[i].normal := Pm3Normal4b(vData)^;
    inc(vData,4); //sizeof(m3Normal4b)

    if hasColor then
    begin
      FVertexArray[i].color := Pm3Color(vData)^;
      inc(vData,4); //sizeof(m3Color)
    end;

    FVertexArray[i].uv0 := Pm3UV(vData)^;
    inc(vData,4); //sizeof(m3UV)

    if hasUV1 then
    begin
      FVertexArray[i].uv1 := Pm3UV(vData)^;
      inc(vData,4); //sizeof(m3UV)
    end;
    if hasUV2 then
    begin
      FVertexArray[i].uv2 := Pm3UV(vData)^;
      inc(vData,4); //sizeof(m3UV)
    end;
    if hasUV3 then
    begin
      FVertexArray[i].uv3 := Pm3UV(vData)^;
      inc(vData,4); //sizeof(m3UV)
    end;

    FVertexArray[i].tangent := Pm3Normal4b(vData)^;
    inc(vData,4); //sizeof(m3Normal4b)
  end;
end;

procedure TF3dView.ShowEditor(const M3: TM3File);
var
  pTag: PM3Structure;
begin
  FM3File := M3;

  pTag := M3.GetModelTag;
  if pTag = nil then exit;
  if not ReadFieldData(pTag^,'vFlags',0,FVertexFlags,4) then Exit;
  pTag := M3.GetVerticesTag;
  if pTag = nil then exit;
  ReadVertexArray(pTag^.Data, pTag^.DataSize);

  Show;
end;

end.

