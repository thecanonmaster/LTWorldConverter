unit uvtoopq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globals, ltworldtypes;

procedure ConvertUVToOPQ(pos: PLTVector; coords: PLTFloat; texWidth: Cardinal; texHeight: Cardinal; O: PLTVector; P: PLTVector; Q: PLTVector);

implementation

function BaryCoordsArea(p0: PLTVector; p1: PLTVector; p2: PLTVector): LTFloat;
var e0, e1: LTVector;
begin
  VEC_SUB(@e0, p1, p0);
  VEC_SUB(@e1, p2, p0);

  Result := e0.x * e1.y - e1.x * e0.y;
end;

function BaryCoords(p0: PLTVector; p1: PLTVector; p2: PLTVector; p: PLTVector): LTVector;
var n, u, v, w: LTFloat;
begin
  n := BaryCoordsArea(p0, p1, p2);

  if abs(n) < 0.001 then
  begin
    Result := LTVectorInit(1.0, 0.0, 0.0);
    Exit;
  end;

  u := BaryCoordsArea(p1, p2, p) / n;
  v := BaryCoordsArea(p2, p0, p) / n;
  w := 1.0 - u - v;

  Result.x := u; Result.y := v; Result.z := w;
end;

procedure ConvertUVToOPQ(pos: PLTVector; coords: PLTFloat; texWidth: Cardinal; texHeight: Cardinal; O: PLTVector; P: PLTVector; Q: PLTVector);
var tv0, tv1, tv2, v0, v1, v2, bcO, bcP, bcQ, vTemp1, vTemp2, vTemp3, vTemp4: LTVector;
    tp, tq, pscale, qscale: LTFloat;
    R, PNew, QNew: LTVector;
begin
  tv0 := LTVectorInit(coords[0], -coords[1], 0.0);
  tv1 := LTVectorInit(coords[2], -coords[3], 0.0);
  tv2 := LTVectorInit(coords[4], -coords[5], 0.0);

  v0 := pos[0];
  v1 := pos[1];
  v2 := pos[2];

  vTemp1 := LTVectorInit(0.0, 0.0, 0.0);
  vTemp2 := LTVectorInit(1.0, 0.0, 0.0);
  vTemp3 := LTVectorInit(0.0, 1.0, 0.0);
  bcO := BaryCoords(@tv0, @tv1, @tv2, @vTemp1);
  bcP := BaryCoords(@tv0, @tv1, @tv2, @vTemp2);
  bcQ := BaryCoords(@tv0, @tv1, @tv2, @vTemp3);

  VEC_MULSCALAR(@vTemp1, @v0, bcO.x);
  VEC_MULSCALAR(@vTemp2, @v1, bcO.y);
  VEC_MULSCALAR(@vTemp3, @v2, bcO.z);
  VEC_ADD(@vTemp4, @vTemp1, @vTemp2);
  VEC_ADD(O, @vTemp4, @vTemp3);

  VEC_MULSCALAR(@vTemp1, @v0, bcP.x);
  VEC_MULSCALAR(@vTemp2, @v1, bcP.y);
  VEC_MULSCALAR(@vTemp3, @v2, bcP.z);
  VEC_ADD(@vTemp4, @vTemp1, @vTemp2);
  VEC_ADD(P, @vTemp4, @vTemp3);

  VEC_MULSCALAR(@vTemp1, @v0, bcQ.x);
  VEC_MULSCALAR(@vTemp2, @v1, bcQ.y);
  VEC_MULSCALAR(@vTemp3, @v2, bcQ.z);
  VEC_ADD(@vTemp4, @vTemp1, @vTemp2);
  VEC_ADD(Q, @vTemp4, @vTemp3);

  VEC_SUB(@vTemp1, P, O);
  P^ := vTemp1;
  VEC_SUB(@vTemp1, Q, O);
  Q^ := vTemp1;

  tp := VEC_MAG(P);
  tp := tp * 1.0 / (LTFloat(texWidth) - 0.5);
  tp := 1.0 / tp;
  tq := VEC_MAG(Q);
  tq := tq * 1.0 / (LTFloat(texHeight) - 0.5);
  tq := 1.0 / tq;

  VEC_NORM(P);
  VEC_NORM(Q);

  VEC_CROSS(@R, Q, P);
  VEC_CROSS(@PNew, @R, Q);
  VEC_CROSS(@QNew, P, @R);

  VEC_NORM(@PNew);
  VEC_NORM(@QNew);
  pscale := 1.0 / VEC_DOT(P, @PNew);
  qscale := 1.0 / VEC_DOT(Q, @QNew);

  VEC_CROSS(@R, @QNew, @PNew);

  VEC_MULSCALAR(@vTemp1, @PNew, tp * pscale);
  PNew := vTemp1;
  VEC_MULSCALAR(@vTemp2, @QNew, tq * qscale);
  QNew := vTemp2;

  VEC_NORM(@R);
  VEC_ADD(P, @PNew, @R);

  VEC_MULSCALAR(@vTemp1, @R, VEC_DOT(@PNew, @QNew));
  VEC_SUB(Q, @QNew, @vTemp1);
end;


end.

