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
unit glmCameraUtils;

{$mode objfpc}{$H+}

interface

uses
  Math, dglOpenGL, glMathUtils;

const
  GLM_CAMERA_VALID               = 0;
  GLM_CAMERA_FAR_CLIP_TOO_CLOSE  = 1;
  GLM_CAMERA_NEAR_CLIP_TOO_CLOSE = 2;
  GLM_CAMERA_FOV_TOO_BIG         = 3;
  GLM_CAMERA_FOV_TOO_SMALL       = 4;
  GLM_CAMERA_TARGET_AT_EYE       = 5;
//GLM_CAMERA_                    = ;

type
  { TglmBaseCamera }

  TglmBaseCamera = class
  private
    FEye: TGLVectorf3;

    FForw: TGLVectorf3;
    FUp: TGLVectorf3;
    FSide: TGLVectorf3;

    FNear: GLfloat;
    FFar: GLfloat;

    FFirstM, FMulM: TglmMatrixf4;
  public
    LastCreatedMatrix: TglmMatrixf4;

    function Validate: Integer; virtual;

    property Eye: TGLVectorf3 read FEye;

    property NearClip: GLfloat read FNear;
    property FarClip: GLfloat read FFar;

    property ForwardVector: TGLVectorf3 read FForw;
    property UpVector: TGLVectorf3 read FUp;
    property SideVector: TGLVectorf3 read FSide;
  end;

  TglmBaseTragetCamera = class (TglmBaseCamera)
  private
    FTarget: TGLVectorf3;
    FDistance: GLfloat;
  public
    property Target: TGLVectorf3 read FTarget;
    property Distance: GLfloat read FDistance;
  end;

  { TglmCustomCamera }

  TglmCustomCamera = class (TglmBaseCamera)
  private
  public
    constructor Create(const zNear, zFar: GLfloat);

    procedure SetNearClip(AValue: GLfloat);
    procedure SetFarClip(AValue: GLfloat);
    procedure SetClips(zNear, zFar: GLfloat);
  end;

  TglmCustomTragetCamera = class (TglmBaseTragetCamera)
  private

  public

  end;

  { TglmFoVTargetCamera }

  TglmFoVTargetCamera = class (TglmCustomTragetCamera)
  private
    FFov: GLfloat;
    FFovtan: GLfloat;

    procedure CalcDistance;
    procedure RecalcNormals;

    procedure SetFar(AValue: GLfloat);
    procedure SetFieldOfVision(AValue: GLfloat);
    procedure SetNear(AValue: GLfloat);
  public
    constructor Create(const fov, znear, zfar: GLfloat);

    procedure LookAt3f(const eyex, eyey, eyez, targetx, targety, targetz, upx, upy, upz: GLfloat);
    procedure LookAt3fv(const veye, vtarget, vup: TGLVectorf3);

    procedure ToOrthoMatrix(const vpW, vpH: Integer; out M: TglmMatrixf4);
    procedure ToPerspectiveMatrix(const vpW, vpH: Integer; out M: TglmMatrixf4);

    procedure SetEye3f(const x, y, z: GLfloat);
    procedure SetEye3fv(const v: TGLVectorf3);

    procedure MoveTarget(const ForwOffset, SideOffset, UpOffset: GLfloat);
    procedure MoveEye(const ForwOffset, SideOffset, UpOffset: GLfloat);
    procedure MoveCamera(const ForwOffset, SideOffset, UpOffset: GLfloat);

    procedure TransTarget(const XOffset, YOffset, ZOffset: GLfloat);
    procedure TransEye(const XOffset, YOffset, ZOffset: GLfloat);
    procedure TransCamera(const XOffset, YOffset, ZOffset: GLfloat);

    procedure RotateEye(const SideAngleRad, UpAngleRad: GLfloat);
    procedure TurnEye(const SideAngleRad, UpAngleRad: GLfloat);

    property FieldOfVision: GLfloat read FFov write SetFieldOfVision;
  end;

implementation

{ TglmBaseCamera }

function TglmBaseCamera.Validate: Integer;
begin
  if FNear >= FFar then
    Exit(GLM_CAMERA_FAR_CLIP_TOO_CLOSE);
  Result := GLM_CAMERA_VALID;
end;

{ TglmCustomCamera }

procedure TglmCustomCamera.SetFarClip(AValue: GLfloat);
begin
  if FFar = AValue then Exit;
  FFar := AValue;
end;

procedure TglmCustomCamera.SetClips(zNear, zFar: GLfloat);
begin
  FNear := zNear;
  FFar := zFar;
end;

procedure TglmCustomCamera.SetNearClip(AValue: GLfloat);
begin
  if FNear = AValue then Exit;
  FNear := AValue;
end;

constructor TglmCustomCamera.Create(const zNear, zFar: GLfloat);
begin
  FNear := zNear;
  FFar := zFar;

  FEye[0]:=0;
  FEye[1]:=0;
  FEye[2]:=2;

  FUp[0]:=0;
  FUp[1]:=1;
  FUp[2]:=0;

  FForw[0] := 0;
  FForw[1] := 0;
  FForw[2] := -1;

  FSide[0] := 1;
  FSide[1] := 0;
  FSide[2] := 0;
end;

{ TglmFoVTargetCamera }

procedure TglmFoVTargetCamera.CalcDistance;
begin
  FDistance:=sqrt(
    sqr(FEye[0]-FTarget[0])+
    sqr(FEye[1]-FTarget[1])+
    sqr(FEye[2]-FTarget[2])
  );
  //if FDistance=0 then
  //  MoveTarget(1,0,0);
end;

procedure TglmFoVTargetCamera.RecalcNormals;
begin
  //------------------
  //Forw = Eye look at Target
  if FDistance>0 then // only when target and eye not at the same point
  begin
    FForw[0] := FTarget[0] - FEye[0];
    FForw[1] := FTarget[1] - FEye[1];
    FForw[2] := FTarget[2] - FEye[2];
  end;
  glmNormalizeVector3fv(@FForw[0]);
  //------------------
  //Side = Forw x Up
  glmComputeNormalOfPlane3fv(@FSide[0], @FForw[0], @FUp[0]);
  glmNormalizeVector3fv(@FSide[0]);
  //------------------
  //Recompute up as: up = side x forw
  glmComputeNormalOfPlane3fv(@FUp[0], @FSide[0], @FForw[0]);
  //glmNormalizeVector3fv(@FSide[0]);
end;

procedure TglmFoVTargetCamera.SetFar(AValue: GLfloat);
begin
  if FFar=AValue then Exit;
  if AValue>FNear then
    FFar:=AValue
  else
    if FDistance>0 then
      FFar:=FNear+FDistance*2
    else
      FFar:=FNear+2;
end;

procedure TglmFoVTargetCamera.SetFieldOfVision(AValue: GLfloat);
begin
  while AValue<0 do AValue:=AValue+180;
  while AValue>180 do AValue:=AValue-180;
  if (FFov=AValue)or(AValue=0)or(AValue=180) then Exit;
  FFov:=AValue;
  FFovtan:=tan(FFov * pi / 360);
end;

procedure TglmFoVTargetCamera.SetNear(AValue: GLfloat);
begin
  if (FNear=AValue)or(AValue<=0) then Exit;
  FNear:=AValue;
  if FNear>FFar then
  begin
    if FDistance>0 then
      FFar:=FNear+FDistance*2
    else
      FFar:=FNear+2;
  end;
end;

constructor TglmFoVTargetCamera.Create(const fov, znear, zfar: GLfloat);
begin
  FTarget[0]:=0;
  FTarget[1]:=0;
  FTarget[2]:=0;
  FEye[0]:=0;
  FEye[1]:=0;
  FEye[2]:=2;
  FDistance:=2;

  FUp[0]:=0;
  FUp[1]:=1;
  FUp[2]:=0;
  RecalcNormals;

  FFov:=90;
  FFovtan:=1;
  SetFieldOfVision(fov);

  FNear:=1;
  SetNear(znear);
  SetFar(zfar);
end;

procedure TglmFoVTargetCamera.LookAt3f(const eyex, eyey, eyez, targetx, targety,
  targetz, upx, upy, upz: GLfloat);
begin
  FTarget[0]:=targetx;
  FTarget[1]:=targety;
  FTarget[2]:=targetz;
  FEye[0]:=eyex;
  FEye[1]:=eyey;
  FEye[2]:=eyez;
  CalcDistance;

  FUp[0]:=upx;
  FUp[1]:=upy;
  FUp[2]:=upz;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.LookAt3fv(const veye, vtarget, vup: TGLVectorf3);
begin
  FTarget:=vtarget;
  FEye:=veye;
  CalcDistance;

  FUp:=vup;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.ToOrthoMatrix(const vpW, vpH: Integer; out M: TglmMatrixf4);
var
  ymax, xmax: GLfloat;
begin
  ymax := FDistance * FFovtan;
  xmax := ymax * (vpW / vpH);
  glmIdntOrthof(@FFirstM[0], -xmax, xmax, -ymax, ymax, FNear, FFar);

  FMulM[m4i0_0] := FSide[0];
  FMulM[m4i1_0] := FSide[1];
  FMulM[m4i2_0] := FSide[2];
  //FMulM[m4i3_0] := 0.0;
  //------------------
  FMulM[m4i0_1] := FUp[0];
  FMulM[m4i1_1] := FUp[1];
  FMulM[m4i2_1] := FUp[2];
  //FMulM[m4i3_1] := 0.0;
  //------------------
  FMulM[m4i0_2] := -FForw[0];
  FMulM[m4i1_2] := -FForw[1];
  FMulM[m4i2_2] := -FForw[2];
  //FMulM[m4i3_2] := 0.0;
  //------------------
  //FMulM[m4i0_3] := 0.0;
  //FMulM[m4i1_3] := 0.0;
  //FMulM[m4i2_3] := 0.0;
  //FMulM[m4i3_3] := 1.0;
  //------------------
  glmMulMatrix4x4Optimized3x3f(@FFirstM[0], @FMulM[0], @M[0]);
  glmTranslatef(@M[0], -FEye[0], -FEye[1], -FEye[2]);
end;

procedure TglmFoVTargetCamera.ToPerspectiveMatrix(const vpW, vpH: Integer; out M: TglmMatrixf4);
var
  ymax, xmax: GLfloat;
begin
  ymax := FNear * FFovtan;
  xmax := ymax * (vpW / vpH);
  glmIdntFrustum(@FFirstM[0], -xmax, xmax, -ymax, ymax, FNear, FFar);

  FMulM[m4i0_0] := FSide[0];
  FMulM[m4i1_0] := FSide[1];
  FMulM[m4i2_0] := FSide[2];
  //FMulM[m4i3_0] := 0.0;
  //------------------
  FMulM[m4i0_1] := FUp[0];
  FMulM[m4i1_1] := FUp[1];
  FMulM[m4i2_1] := FUp[2];
  //FMulM[m4i3_1] := 0.0;
  //------------------
  FMulM[m4i0_2] := -FForw[0];
  FMulM[m4i1_2] := -FForw[1];
  FMulM[m4i2_2] := -FForw[2];
  //FMulM[m4i3_2] := 0.0;
  //------------------
  //FMulM[m4i0_3] := 0.0;
  //FMulM[m4i1_3] := 0.0;
  //FMulM[m4i2_3] := 0.0;
  //FMulM[m4i3_3] := 1.0;
  //------------------
  glmMulMatrix4x4Optimized3x3f(@FFirstM[0], @FMulM[0], @M[0]);
  glmTranslatef(@M[0], -FEye[0], -FEye[1], -FEye[2]);
end;

procedure TglmFoVTargetCamera.SetEye3f(const x, y, z: GLfloat);
begin
  FEye[0]:=x;
  FEye[1]:=y;
  FEye[2]:=z;
  CalcDistance;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.SetEye3fv(const v: TGLVectorf3);
begin
  FEye:=v;
  CalcDistance;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.MoveTarget(const ForwOffset, SideOffset, UpOffset: GLfloat);
begin
  if ForwOffset<>0 then
  begin
    FTarget[0]:=FTarget[0]+FForw[0]*ForwOffset;
    FTarget[1]:=FTarget[1]+FForw[1]*ForwOffset;
    FTarget[2]:=FTarget[2]+FForw[2]*ForwOffset;
  end;
  if SideOffset<>0 then
  begin
    FTarget[0]:=FTarget[0]+FSide[0]*SideOffset;
    FTarget[1]:=FTarget[1]+FSide[1]*SideOffset;
    FTarget[2]:=FTarget[2]+FSide[2]*SideOffset;
  end;
  if UpOffset<>0 then
  begin
    FTarget[0]:=FTarget[0]+FUp[0]*UpOffset;
    FTarget[1]:=FTarget[1]+FUp[1]*UpOffset;
    FTarget[2]:=FTarget[2]+FUp[2]*UpOffset;
  end;
  CalcDistance;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.MoveEye(const ForwOffset, SideOffset, UpOffset: GLfloat);
begin
  if ForwOffset<>0 then
  begin
    FEye[0]:=FEye[0]+FForw[0]*ForwOffset;
    FEye[1]:=FEye[1]+FForw[1]*ForwOffset;
    FEye[2]:=FEye[2]+FForw[2]*ForwOffset;
  end;
  if SideOffset<>0 then
  begin
    FEye[0]:=FEye[0]+FSide[0]*SideOffset;
    FEye[1]:=FEye[1]+FSide[1]*SideOffset;
    FEye[2]:=FEye[2]+FSide[2]*SideOffset;
  end;
  if UpOffset<>0 then
  begin
    FEye[0]:=FEye[0]+FUp[0]*UpOffset;
    FEye[1]:=FEye[1]+FUp[1]*UpOffset;
    FEye[2]:=FEye[2]+FUp[2]*UpOffset;
  end;
  CalcDistance;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.MoveCamera(const ForwOffset, SideOffset, UpOffset: GLfloat);
begin
  if ForwOffset<>0 then
  begin
    FTarget[0]:=FTarget[0]+FForw[0]*ForwOffset;
    FTarget[1]:=FTarget[1]+FForw[1]*ForwOffset;
    FTarget[2]:=FTarget[2]+FForw[2]*ForwOffset;
    FEye[0]:=FEye[0]+FForw[0]*ForwOffset;
    FEye[1]:=FEye[1]+FForw[1]*ForwOffset;
    FEye[2]:=FEye[2]+FForw[2]*ForwOffset;
  end;
  if SideOffset<>0 then
  begin
    FTarget[0]:=FTarget[0]+FSide[0]*SideOffset;
    FTarget[1]:=FTarget[1]+FSide[1]*SideOffset;
    FTarget[2]:=FTarget[2]+FSide[2]*SideOffset;
    FEye[0]:=FEye[0]+FSide[0]*SideOffset;
    FEye[1]:=FEye[1]+FSide[1]*SideOffset;
    FEye[2]:=FEye[2]+FSide[2]*SideOffset;
  end;
  if UpOffset<>0 then
  begin
    FTarget[0]:=FTarget[0]+FUp[0]*UpOffset;
    FTarget[1]:=FTarget[1]+FUp[1]*UpOffset;
    FTarget[2]:=FTarget[2]+FUp[2]*UpOffset;
    FEye[0]:=FEye[0]+FUp[0]*UpOffset;
    FEye[1]:=FEye[1]+FUp[1]*UpOffset;
    FEye[2]:=FEye[2]+FUp[2]*UpOffset;
  end;
  CalcDistance;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.TransTarget(const XOffset, YOffset, ZOffset: GLfloat);
begin
  if XOffset<>0 then
    FTarget[0]:=FTarget[0]+XOffset;
  if YOffset<>0 then
    FTarget[1]:=FTarget[1]+YOffset;
  if ZOffset<>0 then
    FTarget[2]:=FTarget[2]+ZOffset;
  CalcDistance;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.TransEye(const XOffset, YOffset, ZOffset: GLfloat);
begin
  if XOffset<>0 then
    FEye[0]:=FEye[0]+XOffset;
  if YOffset<>0 then
    FEye[1]:=FEye[1]+YOffset;
  if ZOffset<>0 then
    FEye[2]:=FEye[2]+ZOffset;
  CalcDistance;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.TransCamera(const XOffset, YOffset, ZOffset: GLfloat);
begin
  if XOffset<>0 then
  begin
    FTarget[0]:=FTarget[0]+XOffset;
    FEye[0]:=FEye[0]+XOffset;
  end;
  if YOffset<>0 then
  begin
    FTarget[1]:=FTarget[1]+YOffset;
    FEye[1]:=FEye[1]+YOffset;
  end;
  if ZOffset<>0 then
  begin
    FTarget[2]:=FTarget[2]+ZOffset;
    FEye[2]:=FEye[2]+ZOffset;
  end;
  CalcDistance;
  RecalcNormals;
end;

procedure TglmFoVTargetCamera.RotateEye(const SideAngleRad, UpAngleRad: GLfloat);
var
  M: TglmMatrixf4;
  angle: GLfloat;
  rVector, rAxis: TGLVectorf3;
begin
  rVector[0] := FSide[0]*SideAngleRad + FUp[0]*UpAngleRad;
  rVector[1] := FSide[1]*SideAngleRad + FUp[1]*UpAngleRad;
  rVector[2] := FSide[2]*SideAngleRad + FUp[2]*UpAngleRad;
  glmComputeNormalOfPlane3fv(@rAxis[0],@FForw[0],@rVector[0]);
  angle := sqrt(sqr(SideAngleRad)+sqr(UpAngleRad));

  M[m4i0_0] := FSide[0];
  M[m4i1_0] := FSide[1];
  M[m4i2_0] := FSide[2];
  M[m4i3_0] := 0;
  M[m4i0_1] := FUp[0];
  M[m4i1_1] := FUp[1];
  M[m4i2_1] := FUp[2];
  M[m4i3_1] := 0;
  M[m4i0_2] := FForw[0];
  M[m4i1_2] := FForw[1];
  M[m4i2_2] := FForw[2];
  M[m4i3_2] := 0;
  M[m4i0_3] := FEye[0]-FTarget[0];
  M[m4i1_3] := FEye[1]-FTarget[1];
  M[m4i2_3] := FEye[2]-FTarget[2];
  M[m4i3_3] := 1;

  glmRotateRadf(M,angle,rAxis[0],rAxis[1],rAxis[2]);

  FSide[0] := M[m4i0_0];
  FSide[1] := M[m4i1_0];
  FSide[2] := M[m4i2_0];

  FUp[0] := M[m4i0_1];
  FUp[1] := M[m4i1_1];
  FUp[2] := M[m4i2_1];

  FForw[0] := M[m4i0_2];
  FForw[1] := M[m4i1_2];
  FForw[2] := M[m4i2_2];

  FEye[0] := M[m4i0_3]+FTarget[0];
  FEye[1] := M[m4i1_3]+FTarget[1];
  FEye[2] := M[m4i2_3]+FTarget[2];
end;

procedure TglmFoVTargetCamera.TurnEye(const SideAngleRad, UpAngleRad: GLfloat);
var
  M: TglmMatrixf4;
  angle: GLfloat;
  rVector, rAxis: TGLVectorf3;
begin
  rVector[0] := FSide[0]*SideAngleRad + FUp[0]*UpAngleRad;
  rVector[1] := FSide[1]*SideAngleRad + FUp[1]*UpAngleRad;
  rVector[2] := FSide[2]*SideAngleRad + FUp[2]*UpAngleRad;
  glmComputeNormalOfPlane3fv(@rAxis[0],@FForw[0],@rVector[0]);
  angle := sqrt(sqr(SideAngleRad)+sqr(UpAngleRad));

  M[m4i0_0] := FSide[0];
  M[m4i1_0] := FSide[1];
  M[m4i2_0] := FSide[2];
  M[m4i3_0] := 0;
  M[m4i0_1] := FUp[0];
  M[m4i1_1] := FUp[1];
  M[m4i2_1] := FUp[2];
  M[m4i3_1] := 0;
  M[m4i0_2] := FForw[0];
  M[m4i1_2] := FForw[1];
  M[m4i2_2] := FForw[2];
  M[m4i3_2] := 0;
  M[m4i0_3] := FTarget[0]-FEye[0];
  M[m4i1_3] := FTarget[1]-FEye[1];
  M[m4i2_3] := FTarget[2]-FEye[2];
  M[m4i3_3] := 1;

  glmRotateRadf(M,-angle,rAxis[0],rAxis[1],rAxis[2]);

  FSide[0] := M[m4i0_0];
  FSide[1] := M[m4i1_0];
  FSide[2] := M[m4i2_0];

  FUp[0] := M[m4i0_1];
  FUp[1] := M[m4i1_1];
  FUp[2] := M[m4i2_1];

  FForw[0] := M[m4i0_2];
  FForw[1] := M[m4i1_2];
  FForw[2] := M[m4i2_2];

  FTarget[0] := M[m4i0_3]+FEye[0];
  FTarget[1] := M[m4i1_3]+FEye[1];
  FTarget[2] := M[m4i2_3]+FEye[2];
end;

end.

