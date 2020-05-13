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
unit glMathUtils;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
   Math, dglOpenGL;

const
  m4i0_0 = 0;
  m4i0_1 = 1;
  m4i0_2 = 2;
  m4i0_3 = 3;
  m4i1_0 = 4;
  m4i1_1 = 5;
  m4i1_2 = 6;
  m4i1_3 = 7;
  m4i2_0 = 8;
  m4i2_1 = 9;
  m4i2_2 = 10;
  m4i2_3 = 11;
  m4i3_0 = 12;
  m4i3_1 = 13;
  m4i3_2 = 14;
  m4i3_3 = 15;
  m4cnt  = 16;

type
  TglmMatrixf4 = array[m4i0_0..m4i3_3] of GLfloat;

const
  mIdentity: TglmMatrixf4 = (
    1, 0, 0, 0,
    0, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  );

  glm4x4size = sizeof(TglmMatrixf4); // sizeof(GLfloat)*16; // = 64 delphi xe2
  glm4x3size = sizeof(GLfloat)*12;
  glm4x2size = sizeof(GLfloat)*8;
  glm4x1size = sizeof(GLfloat)*4;

procedure glmNormalizeVector2fv(const V: PGLfloat);
procedure glmNormalizeVector3fv(const V: PGLfloat);
procedure glmNormalizeVector4fv(const V: PGLfloat);
procedure glmComputeNormalOfPlane3fv(const normal, pvector1, pvector2: PGLfloat);

procedure glmLoadIdentity(const M: PGLfloat); inline;
procedure glmIdntTranslatef(const M: PGLfloat; const x, y, z: GLfloat); inline;
procedure glmIdntRotateRadf(const M: PGLfloat; const angle, x, y, z: GLfloat);
procedure glmIdntRotateDegf(const M: PGLfloat; const angle, x, y, z: GLfloat);
procedure glmIdntRotateRad3fv(const M: PGLfloat; const angle: GLfloat;
  const RotateVec: PGLfloat);
procedure glmIdntRotateDeg3fv(const M: PGLfloat; const angle: GLfloat;
  const RotateVec: PGLfloat);
procedure glmIdntTranRotateRadf(const M: PGLfloat;
  const tranx, trany, tranz, rotangle, rotx, roty, rotz: GLfloat);
procedure glmIdntTranRotateDegf(const M: PGLfloat;
  const tranx, trany, tranz, rotangle, rotx, roty, rotz: GLfloat);
procedure glmIdntTranRotateRad3fv(const M: PGLfloat; const TranVec: PGLfloat;
  const rotangle: GLfloat; const RotateVec: PGLfloat);
procedure glmIdntTranRotateDeg3fv(const M: PGLfloat; const TranVec: PGLfloat;
  const rotangle: GLfloat; const RotateVec: PGLfloat);

procedure glmIdntRotateTranRadf(const M: PGLfloat;
  const tranx, trany, tranz, rotangle, rotx, roty, rotz: GLfloat);
procedure glmIdntRotateTranDegf(const M: PGLfloat;
  const tranx, trany, tranz, rotangle, rotx, roty, rotz: GLfloat);
procedure glmIdntRotateTranRad3fv(const M: PGLfloat; const TranVec: PGLfloat;
  const rotangle: GLfloat; const RotateVec: PGLfloat);
procedure glmIdntRotateTranDeg3fv(const M: PGLfloat; const TranVec: PGLfloat;
  const rotangle: GLfloat; const RotateVec: PGLfloat);

procedure glmMulMatrix4x4f(const M1, M2, MResult: PGLfloat);
procedure glmMulMatrix4x4Optimized3x3f(const M1, M2, MResult: PGLfloat);

procedure glmIdntFrustum(const M: PGLfloat;
  const left, right, bottom, top, znear, zfar: GLfloat);
procedure glmIdntPerspectiveDeg(const M: PGLfloat;
  const fovy, aspect, znear, zfar: GLfloat);
procedure glmIdntPerspectiveBox(const M: PGLfloat;
  const W, H: Integer; const scale, znear, zfar: GLfloat);

procedure glmFrustum(const M: PGLfloat;
  const left, right, bottom, top, znear, zfar: GLfloat);
procedure glmPerspectiveDeg(const M: PGLfloat;
  const fovy, aspect, znear, zfar: GLfloat);
procedure glmPerspectiveBox(const M: PGLfloat;
  const W, H: Integer; const scale, znear, zfar: GLfloat);

procedure glmIdntOrthof(const M: PGLfloat;
  const left, right, bottom, top, znear, zfar: GLfloat);
procedure glmIdntOrthoBox(const M: PGLfloat;
  const W, H: Integer; const scale, znear, zfar: GLfloat);

procedure glmOrthof(const M: PGLfloat;
  const left, right, bottom, top, znear, zfar: GLfloat);
//procedure glmOrthoConeToBoxDegf(const M: PGLfloat;
//  const fovy, aspect, znear, zfar: GLfloat);
procedure glmOrthoBox(const M: PGLfloat;
  const W, H: Integer; const scale, znear, zfar: GLfloat);

procedure glmLookAt3fv(const M: PGLfloat;
  const eye, center, upVector: PGLfloat);
procedure glmLookAt3f(const M: PGLfloat;
  const eyex, eyey, eyez,
        centerx, centery, centerz,
        upx, upy, upz: GLfloat);

procedure glmTranslatef(const M: PGLfloat; const x, y, z: GLfloat);
procedure glmRotateRadf(const M: PGLfloat; const angle, x, y, z: GLfloat);
procedure glmRotateDegf(const M: PGLfloat; const angle, x, y, z: GLfloat);
procedure glmRotateRad3fv(const M: PGLfloat; const angle: GLfloat;
  const RotateVec: PGLfloat);
procedure glmRotateDeg3fv(const M: PGLfloat; const angle: GLfloat;
  const RotateVec: PGLfloat);

implementation

threadvar
  // trying to use global trheadvar to safely increase performance of
  // glmFrustum, glmLookAt3fv, glmRotateRadf
  // any pros?
  m2, mcopy: TglmMatrixf4;

procedure glmLoadIdentity(const M: PGLfloat); inline;
begin
  //M[m4i0_0]:=1;
  //M[m4i1_1]:=1;
  //M[m4i2_2]:=1;
  //M[m4i3_3]:=1;
  //
  //M[m4i0_1]:=0;
  //M[m4i0_2]:=0;
  //M[m4i0_3]:=0;
  //
  //M[m4i1_0]:=0;
  //M[m4i1_2]:=0;
  //M[m4i1_3]:=0;
  //
  //M[m4i2_0]:=0;
  //M[m4i2_1]:=0;
  //M[m4i2_3]:=0;
  //
  //M[m4i3_0]:=0;
  //M[m4i3_1]:=0;
  //M[m4i3_2]:=0;
  Move(mIdentity,M^,glm4x4size);
end;

procedure glmIdntTranslatef(const M: PGLfloat; const x, y, z: GLfloat); inline;
begin
  //M[m4i0_0]:=1;
  //M[m4i0_1]:=0;
  //M[m4i0_2]:=0;
  //M[m4i0_3]:=0;
  //
  //M[m4i1_0]:=0;
  //M[m4i1_1]:=1;
  //M[m4i1_2]:=0;
  //M[m4i1_3]:=0;
  //
  //M[m4i2_0]:=0;
  //M[m4i2_1]:=0;
  //M[m4i2_2]:=1;
  //M[m4i2_3]:=0;
  Move(mIdentity,m^,glm4x3size);

  m[m4i3_0]:=x;
  m[m4i3_1]:=y;
  m[m4i3_2]:=z;
  m[m4i3_3]:=1;
end;

procedure glmIdntRotateRadf(const M: PGLfloat; const angle, x, y, z: GLfloat);
var
	OneMinusCosAngle, CosAngle, SinAngle,
	X_OneMinusCosAngle, Z_OneMinusCosAngle: GLfloat;
  vec: TGLVectorf3;
begin
  vec[0]:=x;
  vec[1]:=y;
  vec[2]:=z;
  glmNormalizeVector3fv(@vec[0]);

	CosAngle:=Cos(angle); //Some stuff for optimizing code
	OneMinusCosAngle:=1.0-CosAngle;
	SinAngle:=Sin(angle);
	X_OneMinusCosAngle:=vec[0]*OneMinusCosAngle;
	Z_OneMinusCosAngle:=vec[2]*OneMinusCosAngle;

	m[m4i0_0]:=vec[0]*X_OneMinusCosAngle+CosAngle;
	m[m4i0_1]:=vec[1]*X_OneMinusCosAngle+vec[2]*SinAngle;
	m[m4i0_2]:=vec[2]*X_OneMinusCosAngle-vec[1]*SinAngle;
	m[m4i0_3]:=0.0;

	m[m4i1_0]:=vec[1]*X_OneMinusCosAngle-vec[2]*SinAngle;
	m[m4i1_1]:=vec[1]*vec[1]*OneMinusCosAngle+CosAngle;
	m[m4i1_2]:=vec[1]*Z_OneMinusCosAngle+vec[0]*SinAngle;
	m[m4i1_3]:=0.0;

	m[m4i2_0]:=vec[0]*Z_OneMinusCosAngle+vec[1]*SinAngle;
	m[m4i2_1]:=vec[1]*Z_OneMinusCosAngle-vec[0]*SinAngle;
	m[m4i2_2]:=vec[2]*Z_OneMinusCosAngle+CosAngle;
	m[m4i2_3]:=0.0;

  //M[m4i3_0]:=0;
  //M[m4i3_1]:=0;
  //M[m4i3_2]:=0;
  //M[m4i3_3]:=1;
  Move(mIdentity[m4i3_0],M[m4i3_0],glm4x1size);
end;

procedure glmIdntRotateDegf(const M: PGLfloat; const angle, x, y, z: GLfloat);
begin
  glmIdntRotateRadf(M,(angle*Pi)/180.0,x,y,z);
end;

procedure glmIdntRotateRad3fv(const M: PGLfloat; const angle: GLfloat;
  const RotateVec: PGLfloat);
begin
  glmIdntRotateRadf(M,angle,RotateVec[0],RotateVec[1],RotateVec[2]);
end;

procedure glmIdntRotateDeg3fv(const M: PGLfloat; const angle: GLfloat;
  const RotateVec: PGLfloat);
begin
  glmIdntRotateRadf(M,(angle*Pi)/180.0,RotateVec[0],RotateVec[1],RotateVec[2]);
end;

/// Translate Then Rotate ///
procedure glmIdntTranRotateRadf(const M: PGLfloat;
  const tranx, trany, tranz, rotangle, rotx, roty, rotz: GLfloat);
var
	OneMinusCosAngle, CosAngle, SinAngle,
	A_OneMinusCosAngle, C_OneMinusCosAngle: GLfloat;
begin
	CosAngle:=Cos(rotangle); //Some stuff for optimizing code
	OneMinusCosAngle:=1.0-CosAngle;
	SinAngle:=Sin(rotangle);
	A_OneMinusCosAngle:=rotx*OneMinusCosAngle;
	C_OneMinusCosAngle:=rotz*OneMinusCosAngle;

	m[m4i0_0]:=rotx*A_OneMinusCosAngle+CosAngle;
	m[m4i0_1]:=roty*A_OneMinusCosAngle+rotz*SinAngle;
	m[m4i0_2]:=rotz*A_OneMinusCosAngle-roty*SinAngle;
	M[m4i0_3]:=0.0;

	m[m4i1_0]:=roty*A_OneMinusCosAngle-rotz*SinAngle;
	m[m4i1_1]:=roty*roty*OneMinusCosAngle+CosAngle;
	m[m4i1_2]:=roty*C_OneMinusCosAngle+rotx*SinAngle;
	M[m4i1_3]:=0.0;

	m[m4i2_0]:=rotx*C_OneMinusCosAngle+roty*SinAngle;
	m[m4i2_1]:=roty*C_OneMinusCosAngle-rotx*SinAngle;
	m[m4i2_2]:=rotz*C_OneMinusCosAngle+CosAngle;
	M[m4i2_3]:=0.0;

  M[m4i3_0]:=tranx;
  M[m4i3_1]:=trany;
  M[m4i3_2]:=tranz;
  M[m4i3_3]:=1.0;
end;

procedure glmIdntTranRotateDegf(const M: PGLfloat;
  const tranx, trany, tranz, rotangle, rotx, roty, rotz: GLfloat);
begin
  glmIdntTranRotateRadf(M,tranx,trany,tranz,(rotangle*Pi)/180.0,rotx,roty,rotz);
end;

procedure glmIdntTranRotateRad3fv(const M: PGLfloat; const TranVec: PGLfloat;
  const rotangle: GLfloat; const RotateVec: PGLfloat);
begin
  glmIdntTranRotateRadf(M,tranvec[0],tranvec[1],tranvec[2],
    rotangle,RotateVec[0],RotateVec[1],RotateVec[2]);
end;

procedure glmIdntTranRotateDeg3fv(const M: PGLfloat; const TranVec: PGLfloat;
  const rotangle: GLfloat; const RotateVec: PGLfloat);
begin
  glmIdntTranRotateRadf(M,tranvec[0],tranvec[1],tranvec[2],
    (rotangle*Pi)/180.0,RotateVec[0],RotateVec[1],RotateVec[2]);
end;
/// *** End *** ///

/// Rotate Then Translate ///
procedure glmIdntRotateTranRadf(const M: PGLfloat;
  const tranx, trany, tranz, rotangle, rotx, roty, rotz: GLfloat);
var
	OneMinusCosAngle, CosAngle, SinAngle,
	A_OneMinusCosAngle, C_OneMinusCosAngle: GLfloat;
begin
	CosAngle:=Cos(rotangle); //Some stuff for optimizing code
	OneMinusCosAngle:=1.0-CosAngle;
	SinAngle:=Sin(rotangle);
	A_OneMinusCosAngle:=rotx*OneMinusCosAngle;
	C_OneMinusCosAngle:=rotz*OneMinusCosAngle;

	m[m4i0_0]:=rotx*A_OneMinusCosAngle+CosAngle;
	m[m4i0_1]:=roty*A_OneMinusCosAngle+rotz*SinAngle;
	m[m4i0_2]:=rotz*A_OneMinusCosAngle-roty*SinAngle;
	m[m4i0_3]:=0.0;

	m[m4i1_0]:=roty*A_OneMinusCosAngle-rotz*SinAngle;
	m[m4i1_1]:=roty*roty*OneMinusCosAngle+CosAngle;
	m[m4i1_2]:=roty*C_OneMinusCosAngle+rotx*SinAngle;
	m[m4i1_3]:=0.0;

	m[m4i2_0]:=rotx*C_OneMinusCosAngle+roty*SinAngle;
	m[m4i2_1]:=roty*C_OneMinusCosAngle-rotx*SinAngle;
	m[m4i2_2]:=rotz*C_OneMinusCosAngle+CosAngle;
	m[m4i2_3]:=0.0;

	m[m4i3_0] := m[m4i0_0]*tranx + m[m4i1_0]*trany + m[m4i2_0]*tranz;
	m[m4i3_1] := m[m4i0_1]*tranx + m[m4i1_1]*trany + m[m4i2_1]*tranz;
	m[m4i3_2] := m[m4i0_2]*tranx + m[m4i1_2]*trany + m[m4i2_2]*tranz;
	m[m4i3_3] := 1.0;
end;

procedure glmIdntRotateTranDegf(const M: PGLfloat;
  const tranx, trany, tranz, rotangle, rotx, roty, rotz: GLfloat);
begin
  glmIdntRotateTranRadf(M,tranx,trany,tranz,(rotangle*Pi)/180.0,rotx,roty,rotz);
end;

procedure glmIdntRotateTranRad3fv(const M: PGLfloat; const TranVec: PGLfloat;
  const rotangle: GLfloat; const RotateVec: PGLfloat);
begin
  glmIdntRotateTranRadf(M,tranvec[0],tranvec[1],tranvec[2],
    rotangle,RotateVec[0],RotateVec[1],RotateVec[2]);
end;

procedure glmIdntRotateTranDeg3fv(const M: PGLfloat; const TranVec: PGLfloat;
  const rotangle: GLfloat; const RotateVec: PGLfloat);
begin
  glmIdntRotateTranRadf(M,tranvec[0],tranvec[1],tranvec[2],
    (rotangle*Pi)/180.0,RotateVec[0],RotateVec[1],RotateVec[2]);
end;
/// *** End *** ///

procedure glmCopyM(const M: PGLfloat); inline;
begin
  //mcopy[0]:=m[0];
  //mcopy[1]:=m[1];
  //mcopy[2]:=m[2];
  //mcopy[3]:=m[3];
  //mcopy[4]:=m[4];
  //mcopy[5]:=m[5];
  //mcopy[6]:=m[6];
  //mcopy[7]:=m[7];
  //mcopy[8]:=m[8];
  //mcopy[9]:=m[9];
  //mcopy[10]:=m[10];
  //mcopy[11]:=m[11];
  //mcopy[12]:=m[12];
  //mcopy[13]:=m[13];
  //mcopy[14]:=m[14];
  //mcopy[15]:=m[15];
  Move(M^,mcopy,glm4x4size);
end;

procedure glmMulMatrix4x4f(const M1, M2, MResult: PGLfloat);
begin
	MResult[m4i0_0]:=M1[m4i0_0]*M2[m4i0_0]+
                  M1[m4i1_0]*M2[m4i0_1]+
                  M1[m4i2_0]*M2[m4i0_2]+
                  M1[m4i3_0]*M2[m4i0_3];
	MResult[m4i1_0]:=M1[m4i0_0]*M2[m4i1_0]+
                  M1[m4i1_0]*M2[m4i1_1]+
                  M1[m4i2_0]*M2[m4i1_2]+
                  M1[m4i3_0]*M2[m4i1_3];
	MResult[m4i2_0]:=M1[m4i0_0]*M2[m4i2_0]+
                  M1[m4i1_0]*M2[m4i2_1]+
                  M1[m4i2_0]*M2[m4i2_2]+
                  M1[m4i3_0]*M2[m4i2_3];
	MResult[m4i3_0]:=M1[m4i0_0]*M2[m4i3_0]+
                  M1[m4i1_0]*M2[m4i3_1]+
                  M1[m4i2_0]*M2[m4i3_2]+
                  M1[m4i3_0]*M2[m4i3_3];

	MResult[m4i0_1]:=M1[m4i0_1]*M2[m4i0_0]+
                  M1[m4i1_1]*M2[m4i0_1]+
                  M1[m4i2_1]*M2[m4i0_2]+
                  M1[m4i3_1]*M2[m4i0_3];
	MResult[m4i1_1]:=M1[m4i0_1]*M2[m4i1_0]+
                  M1[m4i1_1]*M2[m4i1_1]+
                  M1[m4i2_1]*M2[m4i1_2]+
                  M1[m4i3_1]*M2[m4i1_3];
	MResult[m4i2_1]:=M1[m4i0_1]*M2[m4i2_0]+
                  M1[m4i1_1]*M2[m4i2_1]+
                  M1[m4i2_1]*M2[m4i2_2]+
                  M1[m4i3_1]*M2[m4i2_3];
	MResult[m4i3_1]:=M1[m4i0_1]*M2[m4i3_0]+
                  M1[m4i1_1]*M2[m4i3_1]+
                  M1[m4i2_1]*M2[m4i3_2]+
                  M1[m4i3_1]*M2[m4i3_3];

	MResult[m4i0_2]:=M1[m4i0_2]*M2[m4i0_0]+
                  M1[m4i1_2]*M2[m4i0_1]+
                  M1[m4i2_2]*M2[m4i0_2]+
                  M1[m4i3_2]*M2[m4i0_3];
	MResult[m4i1_2]:=M1[m4i0_2]*M2[m4i1_0]+
                  M1[m4i1_2]*M2[m4i1_1]+
                  M1[m4i2_2]*M2[m4i1_2]+
                  M1[m4i3_2]*M2[m4i1_3];
	MResult[m4i2_2]:=M1[m4i0_2]*M2[m4i2_0]+
                  M1[m4i1_2]*M2[m4i2_1]+
                  M1[m4i2_2]*M2[m4i2_2]+
                  M1[m4i3_2]*M2[m4i2_3];
	MResult[m4i3_2]:=M1[m4i0_2]*M2[m4i3_0]+
                  M1[m4i1_2]*M2[m4i3_1]+
                  M1[m4i2_2]*M2[m4i3_2]+
                  M1[m4i3_2]*M2[m4i3_3];

	MResult[m4i0_3]:=M1[m4i0_3]*M2[m4i0_0]+
                  M1[m4i1_3]*M2[m4i0_1]+
                  M1[m4i2_3]*M2[m4i0_2]+
                  M1[m4i3_3]*M2[m4i0_3];
	MResult[m4i1_3]:=M1[m4i0_3]*M2[m4i1_0]+
                  M1[m4i1_3]*M2[m4i1_1]+
                  M1[m4i2_3]*M2[m4i1_2]+
                  M1[m4i3_3]*M2[m4i1_3];
	MResult[m4i2_3]:=M1[m4i0_3]*M2[m4i2_0]+
                  M1[m4i1_3]*M2[m4i2_1]+
                  M1[m4i2_3]*M2[m4i2_2]+
                  M1[m4i3_3]*M2[m4i2_3];
	MResult[m4i3_3]:=M1[m4i0_3]*M2[m4i3_0]+
                  M1[m4i1_3]*M2[m4i3_1]+
                  M1[m4i2_3]*M2[m4i3_2]+
                  M1[m4i3_3]*M2[m4i3_3];
end;

procedure glmMulMatrix4x4Optimized3x3f(const M1, M2, MResult: PGLfloat);
/// Optimized in case where M2 is like this:
///  x x x 0
///  x x x 0
///  x x x 0
///  0 0 0 1
begin
	MResult[m4i0_0]:=M1[m4i0_0]*M2[m4i0_0]+
                  M1[m4i1_0]*M2[m4i0_1]+
                  M1[m4i2_0]*M2[m4i0_2];
	MResult[m4i1_0]:=M1[m4i0_0]*M2[m4i1_0]+
                  M1[m4i1_0]*M2[m4i1_1]+
                  M1[m4i2_0]*M2[m4i1_2];
	MResult[m4i2_0]:=M1[m4i0_0]*M2[m4i2_0]+
                  M1[m4i1_0]*M2[m4i2_1]+
                  M1[m4i2_0]*M2[m4i2_2];
	MResult[m4i3_0]:=M1[m4i3_0];

	MResult[m4i0_1]:=M1[m4i0_1]*M2[m4i0_0]+
                  M1[m4i1_1]*M2[m4i0_1]+
                  M1[m4i2_1]*M2[m4i0_2];
	MResult[m4i1_1]:=M1[m4i0_1]*M2[m4i1_0]+
                  M1[m4i1_1]*M2[m4i1_1]+
                  M1[m4i2_1]*M2[m4i1_2];
	MResult[m4i2_1]:=M1[m4i0_1]*M2[m4i2_0]+
                  M1[m4i1_1]*M2[m4i2_1]+
                  M1[m4i2_1]*M2[m4i2_2];
	MResult[m4i3_1]:=M1[m4i3_1];

	MResult[m4i0_2]:=M1[m4i0_2]*M2[m4i0_0]+
                  M1[m4i1_2]*M2[m4i0_1]+
                  M1[m4i2_2]*M2[m4i0_2];
	MResult[m4i1_2]:=M1[m4i0_2]*M2[m4i1_0]+
                  M1[m4i1_2]*M2[m4i1_1]+
                  M1[m4i2_2]*M2[m4i1_2];
	MResult[m4i2_2]:=M1[m4i0_2]*M2[m4i2_0]+
                  M1[m4i1_2]*M2[m4i2_1]+
                  M1[m4i2_2]*M2[m4i2_2];
	MResult[m4i3_2]:=M1[m4i3_2];

	MResult[m4i0_3]:=M1[m4i0_3]*M2[m4i0_0]+
                  M1[m4i1_3]*M2[m4i0_1]+
                  M1[m4i2_3]*M2[m4i0_2];
	MResult[m4i1_3]:=M1[m4i0_3]*M2[m4i1_0]+
                  M1[m4i1_3]*M2[m4i1_1]+
                  M1[m4i2_3]*M2[m4i1_2];
	MResult[m4i2_3]:=M1[m4i0_3]*M2[m4i2_0]+
                  M1[m4i1_3]*M2[m4i2_1]+
                  M1[m4i2_3]*M2[m4i2_2];
	MResult[m4i3_3]:=M1[m4i3_3];
end;

procedure glmIdntFrustum(const M: PGLfloat;
  const left, right, bottom, top, znear, zfar: GLfloat);
var
  tmp2N, tmpRmL, tmpTmB, tmpFmN: GLfloat;
begin
  tmp2N := 2.0 * znear;
  tmpRmL := right - left;
  tmpTmB := top - bottom;
  tmpFmN := zfar - znear;

  m[m4i0_0] := tmp2N / tmpRmL;
  m[m4i0_1] := 0.0;
  m[m4i0_2] := 0.0;
  m[m4i0_3] := 0.0;
  m[m4i1_0] := 0.0;
  m[m4i1_1] := tmp2N / tmpTmB;
  m[m4i1_2] := 0.0;
  m[m4i1_3] := 0.0;
  m[m4i2_0] := (right + left) / tmpRmL;
  m[m4i2_1] := (top + bottom) / tmpTmB;
  m[m4i2_2] := (-zfar - znear) / tmpFmN;
  m[m4i2_3] := -1.0;
  m[m4i3_0] := 0.0;
  m[m4i3_1] := 0.0;
  m[m4i3_2] := (-tmp2N * zfar) / tmpFmN;
  m[m4i3_3] := 0.0;
end;

procedure glmIdntPerspectiveDeg(const M: PGLfloat;
  const fovy, aspect, znear, zfar: GLfloat);
var
  ymax, xmax: GLfloat;
begin
    ymax := znear * Tan(fovy * Pi / 360.0);
    xmax := ymax * aspect;
    glmIdntFrustum(m, -xmax, xmax, -ymax, ymax, znear, zfar);
end;

procedure glmIdntPerspectiveBox(const M: PGLfloat;
  const W, H: Integer; const scale, znear, zfar: GLfloat);
var
  ymax, xmax: GLfloat;
begin
    xmax := (W div 2)*scale;
    ymax := (H div 2)*scale;
    glmIdntFrustum(m, -xmax, xmax, -ymax, ymax, znear, zfar);
end;

procedure glmFrustum(const M: PGLfloat;
  const left, right, bottom, top, znear, zfar: GLfloat);
var
  tmp1, tmp2, tmp3, tmp4: GLfloat;
begin
  glmCopyM(M); // copy M to mcopy trheadvar

  tmp1 := 2.0 * znear;
  tmp2 := right - left;
  tmp3 := top - bottom;
  tmp4 := zfar - znear;

  m2[m4i0_0] := tmp1 / tmp2;
  m2[m4i0_1] := 0.0;
  m2[m4i0_2] := 0.0;
  m2[m4i0_3] := 0.0;
  m2[m4i1_0] := 0.0;
  m2[m4i1_1] := tmp1 / tmp3;
  m2[m4i1_2] := 0.0;
  m2[m4i1_3] := 0.0;
  m2[m4i2_0] := (right + left) / tmp2;
  m2[m4i2_1] := (top + bottom) / tmp3;
  m2[m4i2_2] := (-zfar - znear) / tmp4;
  m2[m4i2_3] := -1.0;
  m2[m4i3_0] := 0.0;
  m2[m4i3_1] := 0.0;
  m2[m4i3_2] := (-tmp1 * zfar) / tmp4;
  m2[m4i3_3] := 0.0;

  glmMulMatrix4x4f(@mcopy[0], @m2[0], M);
end;

procedure glmPerspectiveDeg(const M: PGLfloat;
  const fovy, aspect, znear, zfar: GLfloat);
var
  ymax, xmax: GLfloat;
begin
    ymax := znear * Tan(fovy * Pi / 360.0);
    xmax := ymax * aspect;
    glmFrustum(m, -xmax, xmax, -ymax, ymax, znear, zfar);
end;

procedure glmPerspectiveBox(const M: PGLfloat;
  const W, H: Integer; const scale, znear, zfar: GLfloat);
var
  ymax, xmax: GLfloat;
begin
    xmax := (W div 2)*scale;
    ymax := (H div 2)*scale;
    glmFrustum(m, -xmax, xmax, -ymax, ymax, znear, zfar);
end;

procedure glmIdntOrthof(const M: PGLfloat;
  const left, right, bottom, top, znear, zfar: GLfloat);
var
  tmpRmL, tmpTmB, tmpFmN: GLfloat;
begin
  tmpRmL := right - left;
  tmpTmB := top - bottom;
  tmpFmN := zfar - znear;

  m[m4i0_0] := 2.0 / tmpRmL;
  m[m4i0_1] := 0.0;
  m[m4i0_2] := 0.0;
  m[m4i0_3] := 0.0;

  m[m4i1_0] := 0.0;
  m[m4i1_1] := 2.0 / tmpTmB;
  m[m4i1_2] := 0.0;
  m[m4i1_3] := 0.0;

  m[m4i2_0] := 0.0;
  m[m4i2_1] := 0.0;
  m[m4i2_2] := -2.0 / tmpFmN;
  m[m4i2_3] := 0.0;

  m[m4i3_0] := -(right + left) / tmpRmL;
  m[m4i3_1] := -(top + bottom) / tmpTmB;
  m[m4i3_2] := -(zfar + znear) / tmpFmN;
  m[m4i3_3] := 1.0;
end;

procedure glmIdntOrthoBox(const M: PGLfloat;
  const W, H: Integer; const scale, znear, zfar: GLfloat);
var
  ymax, xmax: GLfloat;
begin
    xmax := (W div 2)*scale;
    ymax := (H div 2)*scale;
    glmIdntOrthof(m, -xmax, xmax, -ymax, ymax, znear, zfar);
end;

procedure glmOrthof(const M: PGLfloat;
  const left, right, bottom, top, znear, zfar: GLfloat);
var
  tmpRmL, tmpTmB, tmpFmN: GLfloat;
begin
  glmCopyM(M); // copy M to mcopy trheadvar

  tmpRmL := right - left;
  tmpTmB := top - bottom;
  tmpFmN := zfar - znear;

  m2[m4i0_0] := 2.0 / tmpRmL;
  m2[m4i0_1] := 0.0;
  m2[m4i0_2] := 0.0;
  m2[m4i0_3] := 0.0;

  m2[m4i1_0] := 0.0;
  m2[m4i1_1] := 2.0 / tmpTmB;
  m2[m4i1_2] := 0.0;
  m2[m4i1_3] := 0.0;

  m2[m4i2_0] := 0.0;
  m2[m4i2_1] := 0.0;
  m2[m4i2_2] := -2.0 / tmpFmN;
  m2[m4i2_3] := 0.0;

  m2[m4i3_0] := -(right + left) / tmpRmL;
  m2[m4i3_1] := -(top + bottom) / tmpTmB;
  m2[m4i3_2] := -(zfar + znear) / tmpFmN;
  m2[m4i3_3] := 1.0;

  glmMulMatrix4x4f(@mcopy[0], @m2[0], M);
end;

//procedure glmOrthoConeToBoxDegf(const M: PGLfloat;
//  const fovy, aspect, znear, zfar: GLfloat);
//var
//  ymax, xmax: GLfloat;
//begin
//    ymax := znear * 2 * Tan(fovy * Pi / 360.0);
//    xmax := ymax * aspect;
//    glmOrthof(m, -xmax, xmax, -ymax, ymax, znear, zfar);
//end;

procedure glmOrthoBox(const M: PGLfloat; const W, H: Integer; const scale,
  znear, zfar: GLfloat);
var
  ymax, xmax: GLfloat;
begin
  xmax := (W div 2)*scale;
  ymax := (H div 2)*scale;
  glmOrthof(m, -xmax, xmax, -ymax, ymax, znear, zfar);
end;

procedure glmNormalizeVector2fv(const V: PGLfloat);
var
  tmp: GLfloat;
begin
  tmp := V[0]*V[0] + V[1]*V[1];
  if tmp = 0 then Exit; // No Zero Vectors
  tmp := 1.0/Sqrt(tmp);
  V[0] := V[0]*tmp;
  V[1] := V[1]*tmp;
end;

procedure glmNormalizeVector3fv(const V: PGLfloat);
var
  tmp: GLfloat;
begin
  tmp := V[0]*V[0] + V[1]*V[1] + V[2]*V[2];
  if tmp=0 then Exit; // No Zero Vectors
  tmp := 1.0/Sqrt(tmp);
  V[0] := V[0]*tmp;
  V[1] := V[1]*tmp;
  V[2] := V[2]*tmp;
end;

procedure glmNormalizeVector4fv(const V: PGLfloat);
var
  tmp: GLfloat;
begin
  tmp := V[0]*V[0] + V[1]*V[1] + V[2]*V[2] + V[3]*V[3];
  if (V[0]+V[1]+V[2]+V[3])=0 then Exit; // No Zero Vectors
  tmp := 1.0/Sqrt(tmp);
  V[0] := V[0]*tmp;
  V[1] := V[1]*tmp;
  V[2] := V[2]*tmp;
  V[3] := V[3]*tmp;
end;

procedure glmNormalizeVectorsfv(V: PGLfloat; const size, count: integer);
var
  normal: GLfloat;
  sum: double;
  si, ci: integer;
begin
  for ci := 1 to count do
  begin
    sum:=0;
    for si := 0 to size-1 do
      sum:=sum+V[si]*V[si];
    if sum=0 then
      normal:=0
    else
      normal:=1.0/sqrt(sum);
    for si := 0 to size-1 do
      V[si]:=V[si]*normal;
    inc(V,size);
  end;
end;

procedure glmTranslatef(const M: PGLfloat; const x, y, z: GLfloat);
begin
  m[m4i3_0] := m[m4i0_0]*x + m[m4i1_0]*y + m[m4i2_0]*z + m[m4i3_0];
  m[m4i3_1] := m[m4i0_1]*x + m[m4i1_1]*y + m[m4i2_1]*z + m[m4i3_1];
  m[m4i3_2] := m[m4i0_2]*x + m[m4i1_2]*y + m[m4i2_2]*z + m[m4i3_2];
  m[m4i3_3] := m[m4i0_3]*x + m[m4i1_3]*y + m[m4i2_3]*z + m[m4i3_3];
end;

procedure glmComputeNormalOfPlane3fv(const normal, pvector1, pvector2: PGLfloat);
begin
  normal[0]:=(pvector1[1]*pvector2[2])-(pvector1[2]*pvector2[1]);
  normal[1]:=(pvector1[2]*pvector2[0])-(pvector1[0]*pvector2[2]);
  normal[2]:=(pvector1[0]*pvector2[1])-(pvector1[1]*pvector2[0]);
end;

procedure glmLookAt3fv(const M: PGLfloat;
  const eye, center, upVector: PGLfloat);
var
  forw, side, up: TGLVectorf3;
begin
   glmCopyM(M);
   //------------------
   forw[0] := center[0] - eye[0];
   forw[1] := center[1] - eye[1];
   forw[2] := center[2] - eye[2];
   glmNormalizeVector3fv(@forw[0]); // forw = center -> eye
   //------------------
   //Side = forward x up
   glmComputeNormalOfPlane3fv(@side[0], @forw[0], upVector);
   glmNormalizeVector3fv(@side[0]);
   //------------------
   //Recompute up as: up = side x forward
   glmComputeNormalOfPlane3fv(@up[0], @side[0], @forw[0]);
   //------------------
   //We don't need to set some values because of glmMulMatrix4x4Optimized3x3f optimizations
   m2[m4i0_0] := side[0];
   m2[m4i1_0] := side[1];
   m2[m4i2_0] := side[2];
   //m2[m4i3_0] := 0.0;
   //------------------
   m2[m4i0_1] := up[0];
   m2[m4i1_1] := up[1];
   m2[m4i2_1] := up[2];
   //m2[m4i3_1] := 0.0;
   //------------------
   m2[m4i0_2] := -forw[0];
   m2[m4i1_2] := -forw[1];
   m2[m4i2_2] := -forw[2];
   //m2[m4i3_2] := 0.0;
   //------------------
   //m2[m4i0_3] := 0.0;
   //m2[m4i1_3] := 0.0;
   //m2[m4i2_3] := 0.0;
   //m2[m4i3_3] := 1.0;
   //------------------
   glmMulMatrix4x4Optimized3x3f(@mcopy[0], @m2[0], m);
   glmTranslatef(m, -eye[0], -eye[1], -eye[2]);
end;

procedure glmLookAt3f(const M: PGLfloat;
  const eyex, eyey, eyez,
        centerx, centery, centerz,
        upx, upy, upz: GLfloat);
var
  eye, center, up: TGLVectorf3;
begin
  eye[0]:=eyex;
  eye[1]:=eyey;
  eye[2]:=eyez;
  center[0]:=centerx;
  center[1]:=centery;
  center[2]:=centerz;
  up[0]:=upx;
  up[1]:=upy;
  up[2]:=upz;
  glmLookAt3fv(M, @eye[0], @center[0], @up[0]);
end;

procedure glmRotateRadf(const M: PGLfloat; const angle, x, y, z: GLfloat);
var
	OneMinusCosAngle, CosAngle, SinAngle,
	A_OneMinusCosAngle, C_OneMinusCosAngle: GLfloat;
begin
	CosAngle:=Cos(angle); //Some stuff for optimizing code
	OneMinusCosAngle:=1.0-CosAngle;
	SinAngle:=Sin(angle);
	A_OneMinusCosAngle:=x*OneMinusCosAngle;
	C_OneMinusCosAngle:=z*OneMinusCosAngle;
	//Make a copy
  glmCopyM(M);

	m2[m4i0_0]:=x*A_OneMinusCosAngle+CosAngle;
	m2[m4i0_1]:=y*A_OneMinusCosAngle+z*SinAngle;
	m2[m4i0_2]:=z*A_OneMinusCosAngle-y*SinAngle;
	m2[m4i0_3]:=0.0;

	m2[m4i1_0]:=y*A_OneMinusCosAngle-z*SinAngle;
	m2[m4i1_1]:=y*y*OneMinusCosAngle+CosAngle;
	m2[m4i1_2]:=y*C_OneMinusCosAngle+x*SinAngle;
	m2[m4i1_3]:=0.0;

	m2[m4i2_0]:=x*C_OneMinusCosAngle+y*SinAngle;
	m2[m4i2_1]:=y*C_OneMinusCosAngle-x*SinAngle;
	m2[m4i2_2]:=z*C_OneMinusCosAngle+CosAngle;
	m2[m4i2_3]:=0.0;
	//The last column of m2[] is {0 0 0 1}
  m2[m4i3_0]:=0.0;
  m2[m4i3_1]:=0.0;
  m2[m4i3_2]:=0.0;
  m2[m4i3_3]:=1.0;

  glmMulMatrix4x4f(@mcopy[0],@m2[0],M);
end;

procedure glmRotateDegf(const M: PGLfloat; const angle, x, y, z: GLfloat);
begin
  glmRotateRadf(M,(angle*Pi)/180.0,x,y,z);
end;

procedure glmRotateRad3fv(const M: PGLfloat; const angle: GLfloat;
  const RotateVec: PGLfloat);
begin
  glmRotateRadf(M,angle,RotateVec[0],RotateVec[1],RotateVec[2]);
end;

procedure glmRotateDeg3fv(const M: PGLfloat; const angle: GLfloat;
  const RotateVec: PGLfloat);
begin
  glmRotateRadf(M,(angle*Pi)/180.0,RotateVec[0],RotateVec[1],RotateVec[2]);
end;

end.
