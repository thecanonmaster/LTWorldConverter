unit ltworldtypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

const
  BSP_PHYSICS = 'PhysicsBSP';
  BSP_VIS = 'VisBSP';

  PT_STRING = 0;
  PT_VECTOR = 1;
  PT_COLOR = 2;
  PT_REAL = 3;
  PT_FLAGS = 4;
  PT_BOOL = 5;
  PT_LONGINT = 6;
  PT_ROTATION = 7;

  NF_IN = 1;
  NF_OUT = 2;

  NFI_NODE_IN = 0;
  NFI_NODE_OUT = 1;
  NFI_ERROR = 3;
  NFI_OK = 4;

  PLANE_POSX = 0;
  PLANE_NEGX = 1;
  PLANE_POSY = 2;
  PLANE_NEGY = 3;
  PLANE_POSZ = 4;
  PLANE_NEGZ = 5;
  PLANE_GENERIC = 6;

  PLANE_EP = 0.99999;

  MAX_WORLDPOLY_VERTS = 40;

  // surface flags
  SURF_SOLID			= (1 shl 0);		// Solid.
  SURF_NONEXISTANT		= (1 shl 1);		// Gets removed in preprocessor.
  SURF_INVISIBLE		= (1 shl 2);		// Don't draw.
  SURF_TRANSPARENT		= (1 shl 3);		// Translucent.
  SURF_SKY			= (1 shl 4);		// Sky portal.
  SURF_BRIGHT			= (1 shl 5);		// Fully bright.
  SURF_FLATSHADE		= (1 shl 6);		// Flat shade this poly.
  SURF_LIGHTMAP			= (1 shl 7);		// Lightmap this poly.
  SURF_NOSUBDIV			= (1 shl 8);		// Don't subdivide the poly.
  SURF_HULLMAKER		= (1 shl 9);		// Adds hulls to make PVS better for open areas.
  SURF_ALWAYSLIGHTMAP		= (1 shl 10);		// Override for preprocessor's -allgouraud and -lightmaptogouraud.
  SURF_DIRECTIONALLIGHT	        = (1 shl 11);		// This surface is only lit by the GlobalDirLight.
  SURF_GOURAUDSHADE		= (1 shl 12);		// Gouraud shade this poly.
  SURF_PORTAL			= (1 shl 13);		// This surface defines a portal that can be opened/closed.
  SURF_SPRITEANIMATE		= (1 shl 14);		// This surface's texture is animated by a sprite.
  SURF_PANNINGSKY		= (1 shl 15);		// This surface has the panning sky overlaid on it.
  SURF_XZONLY			= (1 shl 16);		// Only DEdit cares about this.
  SURF_PHYSICSFIX		= (1 shl 17);		// A dummy poly used to protect objects from moving through
  							// certain non axis-aligned edges.
  SURF_TERRAINOCCLUDER	        = (1 shl 18);		// Used for visibility calculations on terrain.
  SURF_ADDITIVE			= (1 shl 19);		// Add source and dest colors.
  SURF_TIMEOFDAY		= (1 shl 20);		// Uses time of day stuff ambient and directional light;.
  SURF_VISBLOCKER		= (1 shl 21);		// Blocks off the visibility tree
  SURF_NOTASTEP			= (1 shl 22);		// Don't try to step up onto this polygon
  SURF_NOWALLWALK		= (1 shl 23);		// Don't do sphere orientation collision for this surface
  SURF_NOBLOCKLIGHT		= (1 shl 24);		// Don't block light with this surface

type
  LTFloat = Single;
  LTVector = record
    x: LTFloat;
    y: LTFloat;
    z: LTFloat;
  end;

  LTRotation = record
    x: LTFloat;
    y: LTFloat;
    z: LTFloat;
    w: LTFloat;
  end;

  TUVPair = record
    a: LTFloat;
    b: LTFloat;
  end;

  PLTVector = ^LTVector;
  PLTRotation = ^LTRotation;
  PLTFloat = ^LTFloat;
  PUVPair = ^TUVPair;

  TLTUnknownStruct1 = record
    vVec1: LTVector;
    vVec2: LTVector;
    nWord1: Word;
  end;

  TWorldHeader = record
    nVersion: Cardinal;
    dwObjectDataPos: Cardinal;
    dwRenderDataPos: Cardinal;
    // addtional
    dwDummy1: Cardinal;
    dwDummy2: Cardinal;
    dwDummy3: Cardinal;
    dwDummy4: Cardinal;
    dwDummy5: Cardinal;
    dwDummy6: Cardinal;
    dwDummy7: Cardinal;
    dwDummy8: Cardinal;
  end;

  TWorldExtents = record
    fUnknown: LTFloat;
    vExtentsMin: LTVector;
    vExtentsMax: LTVector;
    vOffset: LTVector;
  end;

  TWorldObjectList = record
    nNumObjects: Cardinal;
    pObjectList: TFPObjectList;
  end;

  TWorldModelList = record
    nNumModels: Cardinal;
    pModelList: TFPObjectList;
  end;

  TWorldLMAnimList = record
    nNumLMAnims: Cardinal;
    pLMAnimList: TFPObjectList;
  end;

function LTVectorToStrC(V: PLTVector): string;
function LTRotationIgnoreWToStrC(R: PLTRotation): string;
function LTRotationToStrC(R: PLTRotation): string;
function LTVectorInit(x, y, z: LTFloat): LTVector;
function LTRotationInit(x, y, z, w: LTFloat): LTRotation;
function UVToStrC(U: PUVPair): string;
function FlagsToBool(dwFlags: Cardinal; dwWhat: Cardinal; bUseNot: Boolean = False): Byte;
procedure VEC_SUB(d: PLTVector; v1: PLTVector; v2: PLTVector);
function VEC_MAGSQR(v: PLTVector): LTFloat;
function VEC_MAG(v: PLTVector): LTFloat;
procedure VEC_NORM(v: PLTVector);
procedure VEC_CROSS(dest: PLTVector; v1: PLTVector; v2: PLTVector);
procedure VEC_MULSCALAR(d: PLTVector; v1: PLTVector; s: LTFloat);
procedure VEC_ADD(d: PLTVector; v1: PLTVector; v2: PLTVector);
function VEC_DOT(v1: PLTVector; v2: PLTVector): LTFloat;

implementation


function LTVectorInit(x, y, z: LTFloat): LTVector;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function LTRotationInit(x, y, z, w: LTFloat): LTRotation;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function LTVectorToStrC(V: PLTVector): string;
begin
  Result := FormatFloat('0.000000', V^.x) + ' ' +
           FormatFloat('0.000000', V^.y) + ' ' +
           FormatFloat('0.000000', V^.z);
end;

function LTRotationIgnoreWToStrC(R: PLTRotation): string;
begin
  Result := FormatFloat('0.000000', R^.x) + ' ' +
           FormatFloat('0.000000', R^.y) + ' ' +
           FormatFloat('0.000000', R^.z);
end;

function LTRotationToStrC(R: PLTRotation): string;
begin
  Result := FormatFloat('0.000000', R^.x) + ' ' +
           FormatFloat('0.000000', R^.y) + ' ' +
           FormatFloat('0.000000', R^.z) + ' ' +
            FormatFloat('0.000000', R^.w);
end;

function UVToStrC(U: PUVPair): string;
begin
  Result := FormatFloat('0.000000', U^.a) + ' ' +
           FormatFloat('0.000000', U^.b);
end;

function FlagsToBool(dwFlags: Cardinal; dwWhat: Cardinal; bUseNot: Boolean): Byte;
var dwTemp: Cardinal;
begin
  dwTemp := dwFlags and dwWhat;
  if bUseNot then
  begin
    if dwTemp = 0 then Result := 1
    else Result := 0;
  end
  else
  begin
    if dwTemp = 0 then Result := 0
    else Result := 1;
  end;
end;

procedure VEC_SUB(d: PLTVector; v1: PLTVector; v2: PLTVector);
begin
  d^.x := v1^.x - v2^.x;
  d^.y := v1^.y - v2^.y;
  d^.z := v1^.z - v2^.z;
end;

function VEC_MAGSQR(v: PLTVector): LTFloat;
begin
  Result := v^.x * v^.x + v^.y * v^.y + v^.z * v^.z;
end;

function VEC_MAG(v: PLTVector): LTFloat;
begin
  Result := sqrt(VEC_MAGSQR(v));
end;

procedure VEC_NORM(v: PLTVector);
var fTemp: LTFloat;
begin
  fTemp := 1.0 / VEC_MAG(v);
  v^.x := v^.x * fTemp;
  v^.y := v^.y * fTemp;
  v^.z := v^.z * fTemp;
end;

procedure VEC_CROSS(dest: PLTVector; v1: PLTVector; v2: PLTVector);
begin
  dest^.x := v2^.y * v1^.z - v2^.z * v1^.y;
  dest^.y := v2^.z * v1^.x - v2^.x * v1^.z;
  dest^.z := v2^.x * v1^.y - v2^.y * v1^.x;
end;

procedure VEC_MULSCALAR(d: PLTVector; v1: PLTVector; s: LTFloat);
begin
  d^.x := v1^.x * s;
  d^.y := v1^.y * s;
  d^.z := v1^.z * s;
end;

procedure VEC_ADD(d: PLTVector; v1: PLTVector; v2: PLTVector);
begin
  d^.x := v1^.x + v2^.x;
  d^.y := v1^.y + v2^.y;
  d^.z := v1^.z + v2^.z;
end;

function VEC_DOT(v1: PLTVector; v2: PLTVector): LTFloat;
begin
  Result := v1^.x * v2^.x + v1^.y * v2^.y + v1^.z * v2^.z;
end;

end.

