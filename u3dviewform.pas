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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uM3File, ustructures, dglOpenGL, glMathUtils, glmCameraUtils, Types;

type

  { TF3dView }

  TF3dView = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Panel3D: TPanel;
    PanelRight: TPanel;
    PanelLeft: TPanel;
    PanelBottom: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel3DMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel3DMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Panel3DMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  private
    FM3File: TM3File;
    FVertexFlags: UInt32;
    FVertexArray: array of TM3VertexInfoFull;
    FVertexCount: Integer;

    FFacesData: Pointer;
    FFacesCount: Integer;

    FRegionsTag: PM3Structure;
    FObjectsTag: PM3Structure;
    FFacesTag: PM3Structure;
    FBonesTag: PM3Structure;

    FGLVertexArray: GLuint;
    FGLVertexBuffer: GLuint;
    FGLFacesBuffer: GLuint;

    FCamera: TglmFoVTargetCamera;
    FCameraMatrix: TglmMatrixf4;
    FViewPort: TPoint;

    FIsMoveMode: Boolean;
    FMouseX: Integer;
    FMouseY: Integer;

    FVertShaderFile: string;
    FFragShaderFile: string;
    FGLProgram: GLHandle;
    FUniformMVP: GLint;

    procedure ReadVertexArray(vData: Pointer; DataSize: Integer);

    procedure GLWndInit;
    procedure GLWndFinal;
    procedure ProcessModelData;
  public
    procedure ShowEditor(const M3: TM3File);

    procedure FrameStart;
    procedure FrameRender;
  end;

var
  F3dView: TF3dView;

implementation

uses
  uCommon, umain, RenderUtils, RenderWnd, RenderEx;

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

procedure TF3dView.FormCreate(Sender: TObject);
begin
  FCamera := TglmFoVTargetCamera.Create(45,1,500);
  FViewPort.x := Panel3D.Width;
  FViewPort.y := Panel3D.Height;
  FVertShaderFile := IniMain.ReadString('3dview','vert','modelEdit.vert');
  FFragShaderFile := IniMain.ReadString('3dview','frag','modelEdit.frag');
end;

procedure TF3dView.Button1Click(Sender: TObject);
begin
  FCamera.TransCamera(0,0,0.2);
end;

procedure TF3dView.Button2Click(Sender: TObject);
begin
  FCamera.TransCamera(0,0,-0.2);
end;

procedure TF3dView.Button3Click(Sender: TObject);
begin
  FCamera.LookAt3f(
    5,5,5,
    0,0,0,
    0,0,1
  );
end;

procedure TF3dView.Button4Click(Sender: TObject);
var
  i: integer;
  str: TStringList;
  s: string;
begin
  str := TStringList.Create;
  for i := 0 to FVertexCount do
  begin
    s := IntToStr(FVertexArray[i].normal.sign);
    if str.IndexOf(s)=-1 then
      str.Add(s);
  end;
  FMain.Log('normals w:'+str.Text);
  str.Free;
end;

procedure TF3dView.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TF3dView.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin

end;

procedure TF3dView.FormDestroy(Sender: TObject);
begin
  FMain.Free3DEditMode;
  FCamera.Free;
end;

procedure TF3dView.Panel3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsMoveMode := True;
  FMouseX := X;
  FMouseY := Y;
end;

procedure TF3dView.Panel3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FIsMoveMode then
  begin
    FCamera.RotateEye((X-FMouseX)*0.01,(Y-FMouseY)*0.01);
    FMouseX := X;
    FMouseY := Y;
  end;
end;

procedure TF3dView.Panel3DMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FIsMoveMode := False;
end;

procedure TF3dView.Panel3DMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  FCamera.MoveEye(-0.2,0,0);
  Handled := True
end;

procedure TF3dView.Panel3DMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  FCamera.MoveEye(0.2,0,0);
  Handled := True
end;

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
  FVertexCount := DataSize div vSize;
  SetLength(FVertexArray,FVertexCount);
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

procedure TF3dView.GLWndInit;
begin
  glClearColor(0,0,0.4,0);
  if not LoadPorgram(FVertShaderFile,FFragShaderFile,FGLProgram) then
  begin
    FMain.Log('Error creating shader program!');
    FMain.Log(GetLastShaderLog);
    Exit;
  end;
  FUniformMVP := glGetUniformLocation(FGLProgram, 'MVP');

  glGenVertexArrays(1,@FGLVertexArray);
  glBindVertexArray(FGLVertexArray);

  glGenBuffers(1,@FGLVertexBuffer);
  glBindBuffer(GL_ARRAY_BUFFER,FGLVertexBuffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(TM3VertexInfoFull)*length(FVertexArray), @FVertexArray[0], GL_STATIC_DRAW);

  glGenBuffers(1,@FGLFacesBuffer);

  FCamera.LookAt3f(
    5,5,5,
    0,0,0,
    0,0,1
  );
end;

procedure TF3dView.GLWndFinal;
begin
  if FGLProgram = 0 then Exit;
  glDeleteProgram(FGLProgram);
  glDeleteBuffers(1,@FGLVertexBuffer);
  glDeleteBuffers(1,@FGLFacesBuffer);
  glDeleteVertexArrays(1,@FGLVertexArray);
end;

procedure TF3dView.ProcessModelData;
begin
  glBindVertexArray(FGLVertexArray);
  glBindBuffer(GL_ARRAY_BUFFER,FGLVertexBuffer);
  glBufferData(GL_ARRAY_BUFFER, sizeof(TM3VertexInfoFull)*length(FVertexArray), @FVertexArray[0], GL_STATIC_DRAW);
end;

procedure TF3dView.ShowEditor(const M3: TM3File);
var
  pModl, pTag: PM3Structure;
begin
  FM3File := M3;

  pModl := M3.GetModelTag;
  if pModl = nil then exit;
  if not ReadFieldData(pModl^,'vFlags',0,FVertexFlags,4) then Exit;

  pTag := M3.GetVerticesTag;
  if pTag = nil then exit;
  ReadVertexArray(pTag^.Data, pTag^.DataSize);

  pTag := FM3File.FollowRefField(pModl^,0,'divisions');
  if pTag = nil then exit;
  FRegionsTag := FM3File.FollowRefField(pTag^,0,'regions');
  FObjectsTag := FM3File.FollowRefField(pTag^,0,'objects');

  FFacesTag := FM3File.FollowRefField(pTag^,0,'faces');
  if FFacesTag = nil then
  begin
    FFacesData := nil;
    FFacesCount := 0;
  end
  else
  begin
    FFacesData := FFacesTag^.Data;
    FFacesCount := FFacesTag^.ItemCount;
  end;

  FBonesTag := FM3File.FollowRefField(pModl^,0,'bones');

  if not CreateRenderWindow(Panel3D.Handle,0,@GLWndInit,@GLWndFinal) then
    FMain.Log('Failed to create render window (%d)',[GetLastOSError]);
  Show;
end;

procedure TF3dView.FrameStart;
begin
  if FGLProgram = 0 then Exit;
  FCamera.ToPerspectiveMatrix(Panel3D.Width,Panel3D.Height,FCameraMatrix);
end;

procedure TF3dView.FrameRender;
var
  mvp: TglmMatrixf4;
begin
  if FGLProgram = 0 then Exit;
  glClear(GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT);
  glUseProgram(FGLProgram);
  glUniformMatrix4fv(FUniformMVP,1,GL_FALSE,@FCameraMatrix[0]);

  glBindVertexArray(FGLVertexArray);
  glEnableVertexAttribArray(0);
  glBindBuffer(GL_ARRAY_BUFFER,FGLVertexBuffer);
  glVertexAttribPointer(
    0, // position
    3,
    GL_FLOAT,
    GL_FALSE,
    sizeof(TM3VertexInfoFull),
    PGLvoid(0)
  );

  glDrawArrays(GL_POINTS,0,FVertexCount);

  glDisableVertexAttribArray(0);
end;

end.

