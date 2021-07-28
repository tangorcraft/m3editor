unit UToolFixBoneScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCommon, ustructures, uM3File;

procedure FixNegativeBoneScale(const M3: TM3File);

implementation

uses
  umain, glMathUtils, dglOpenGL, UAnimListView;

type
  PAnimationRefVec3 = ^TAnimationRefVec3;
  TAnimationRefVec3 = packed record
    interpolationType: UInt16;
    animFlags: UInt16;
    animId: UInt32;
    initVal: m3VEC3;
    nullVal: m3VEC3;
    unk: Int32;
  end;

procedure FixNegativeBoneScale(const M3: TM3File);
var
  i, j, k, k2: Integer;
  pBones, pIRef, pSTC, pTmp, pRef: PM3Structure;
  animRef: PAnimationRefVec3;
  vec3: m3VEC3;
  pM: PGLfloat;
  refIdx, refType: Int32;
  sdRef: Pm3ref;
  sdData: PAnimSequenceData;
  needFix: boolean;
begin
  pTmp := m3.GetModelTag;
  if not Assigned(pTmp) or not Structures.GetStructureInfo(pTmp^) then Exit;
  pBones := m3.FollowRefField(pTmp^,0,'bones');
  if not Assigned(pBones) or not Structures.GetStructureInfo(pBones^) then Exit;
  pIRef := m3.FollowRefField(pTmp^,0,'absoluteInverseBoneRestPositions');
  if not Assigned(pIRef) or not Structures.GetStructureInfo(pIRef^) then Exit;
  pSTC := m3.FollowRefField(pTmp^,0,'sequenceTransformationCollections');
  if not Assigned(pSTC) or not Structures.GetStructureInfo(pSTC^) then Exit;

  for i := 0 to pBones^.ItemCount-1 do
  begin
    animRef := GetFieldPointer(pBones^,'scale',i);
    needFix := false;
    if animRef^.initVal.x < 0 then
    begin
      vec3.x := -1;
      needFix := true;
    end
    else
      vec3.x := 1;
    if animRef^.initVal.y < 0 then
    begin
      vec3.y := -1;
      needFix := true;
    end
    else
      vec3.y := 1;
    if animRef^.initVal.z < 0 then
    begin
      vec3.z := -1;
      needFix := true;
    end
    else
      vec3.z := 1;

    if not needFix then Continue;

    FMain.Log('Fixing %d:BONE[%d]',[pBones^.Index,i]);
    with animRef^.initVal do
    begin
      x := x*vec3.x;
      y := y*vec3.y;
      z := z*vec3.z;
    end;

    with pIRef^ do
      pM := Data + ItemSize*i;
    glmScalef(pM,vec3.x,vec3.y,vec3.z);

    for j := 0 to pSTC^.ItemCount-1 do
    begin
      pTmp := m3.FollowRefField(pSTC^,j,'animIds');
      pRef := m3.FollowRefField(pSTC^,j,'animRefs');
      if Assigned(pTmp) then
        for k := 0 to pTmp^.ItemCount-1 do
        begin
          if pUInt32(pTmp^.Data+pTmp^.ItemSize*k)^ = animRef^.animId then
          begin
            refIdx := pUInt32(pRef^.Data+k*pRef^.ItemSize)^;
            refType := (refIdx shr 16) and $FFFF;
            refIdx := refIdx and $FFFF;
            sdRef := GetFieldPointer(pSTC^,'sdev',j)+sizeof(m3ref)*refType;
            pTmp := M3[sdRef^.refIndex];
            sdData := pTmp^.Data+pTmp^.ItemSize*refIdx;
            pTmp := M3[sdData^.keys.refIndex];

            for k2 := 0 to pTmp^.ItemCount do
            with Pm3VEC3(pTmp^.Data+pTmp^.ItemSize*k2)^ do
            begin
              x := x*vec3.x;
              y := y*vec3.y;
              z := z*vec3.z;
            end;

            Break;
          end;
        end;
    end;
  end;
end;

end.

