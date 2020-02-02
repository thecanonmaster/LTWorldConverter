unit ltaworldexporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ltworldreader, ltworldtypes, ltworldobject, contnrs,
  ltworlddata, globals, MyLogger;

const
  
  // root
  ARRAY_WORLDHEADER = ' world ';
  
  // header
  NODE_HEADER = ' header ';
  {begin}
    PROP_VERSIONCODE = ' versioncode ';
    PROP_INFOSTRING = ' infostring ';
  {end}

  // polyhedrons
  NODE_POLYHEDRONLIST = ' polyhedronlist ';
  {begin}
    NODE_POLYHEDRON = ' polyhedron ';
    PROP_COLOR2 = ' color';
    ARRAY_POINTLIST = ' pointlist ';
    NODE_POLYLIST = ' polylist ';
    {begin}
      ARRAY_EDITPOLY = ' editpoly ';
      PROP_F = ' f';
      PROP_N = ' n ';
      PROP_DIST = ' dist ';
      ARRAY_TEXTUREINFO = ' textureinfo ';
      {begin}
        PROP_STICKTOPOLY = ' sticktopoly ';
        //PROP_NAME = ' name ';
        PROP_FLAG = ' flag ';
        PROP_SHADE = ' shade';
        PROP_PHYSICSMATERIAL = ' physicsmaterial ';
        PROP_SURFACEKEY = ' surfacekey ';
        NODE_TEXTURES = ' textures ';
        {begin}
          // nothing special here
        {end}
      {end}
    {end}
  {end}
  
  // proplist
  NODE_GLOBALPROPLIST = ' globalproplist ';
  {begin}
    NODE_PROPLIST = ' proplist ';
	{begin}
	  PROP_STRING = ' string ';
	  PROP_VECTOR = ' vector ';
	  PROP_ROTATION = ' rotation ';
	  PROP_REAL = ' real ';
	  PROP_LONGINT = ' longint ';
	  PROP_BOOL = ' bool ';
	  PROP_COLOR = ' color ';
	  {begin}
	    SPEC_GROUP = 'group';
	    SPEC_GROUPOWNER = 'groupowner';
	    SPEC_DISTANCE = 'distance';
	    SPEC_DATA = 'data';
	    SPEC_STATICLIST = 'staticlist';
	    SPEC_NOTIFYCHANGE = 'notifychange';
	    SPEC_EULERANGLES = 'eulerangles';
	    SPEC_TEXTUREEFFECT = 'textureeffect';
	    SPEC_HIDDEN = 'hidden';
	    SPEC_RADIUS = 'radius';
	    SPEC_DIMS = 'dims';
	    SPEC_LOCALDIMS = 'localdims';
	  {end}
	{end}
  {end}
  
  // world nodes
  ARRAY_HIERARCHY = ' nodehierarchy ';
  {begin}
    ARRAY_WORLDNODE = ' worldnode ';
	{begin}
	  PROP_TYPE = ' type ';
          PROP_BRUSHINDEX = ' brushindex ';
	  PROP_LABEL = ' label ';
	  PROP_NODEID = ' nodeid ';
	  PROP_FLAGS = ' flags ';
	  {begin}
	    SPEC_WORLDROOT = 'worldroot';
	    SPEC_EXPANDED = 'expanded';
	  {end}
	  ARRAY_PROPERTIES = ' properties ';
	  {begin}
	    PROP_NAME = ' name ';
	    PROP_PROPID = ' propid ';
	  {end}
	  NODE_CHILDLIST = ' childlist ';
	{end}
  {end}
  
  // navigator pos list
  NODE_NAVIGATORPOSLIST = ' navigatorposlist ';

  // other consts
  BT_AVP2 = 0;
  BT_NOLF2 = 1;
  BGT_SIMPLE = 0;
  BGT_POLY = 1;

type
  TLTWorldBrush = class(TLTWorldObject)
  private
    m_szBspName: string;
  public
    property BspName: string read m_szBspName write m_szBspName;
  end;

  { TLTAWorldExporter }

  TLTAWorldExporter = class(TObject)
  protected
    m_slExport: TStringList;
    m_slClassList: TStringList;
    m_slModelList: TStringList;
    m_pReader: TLTWorldReader;
    //m_pMyCP: TMyCrossPlatform;
    //m_pLogger: TMyLameLogger;
    m_pBrushPropList: TFPObjectList;

    m_nBrushType: Integer;
    m_nBrushGetType: Integer;
    m_szMainGeometrySource: String;

    // generic data writers
    procedure WriteNodeStart(Level: Integer; ID: string; Name: string);
    procedure WriteNodeEnd(Level: Integer);
    procedure WriteArrayStart(Level: Integer; ID: string; Name: string);
    procedure WriteArrayEnd(Level: Integer);

    procedure WriteGenericProp(Level: Integer; ID: string; Value: string);
    procedure WriteGenericSet(Level: Integer; Args: array of string);
    procedure WriteGenericPropStr(Level: Integer; ID: string; Value: string);
    procedure WriteArrayVector(Level: Integer; ID: string; Vector: PLTVector);
    procedure WriteVector(Level: Integer; Vector: PLTVector);
    procedure WriteWorldPoint(Level: Integer; Vector: PLTVector);
    procedure WriteArrayRotation(Level: Integer; ID: string; Rotation: PLTRotation);
    procedure WriteRotation(Level: Integer; Rotation: PLTRotation);
    procedure WriteLineComment(Level: Integer; Text: string);

    // specific writers
    procedure WriteGenericPropExt(Level: Integer; DataType: string; ID: string; ExtData: string; Value: string);

    // data writers
    procedure WriteLTWorld;
    procedure WriteHeader(Level: Integer);
    procedure WritePolyhedronList(Level: Integer);
    procedure WriteNodeHierarchy(Level: Integer);
    procedure WriteGlobalPropList(Level: Integer);
    procedure WritePropList(Level: Integer; nIndex: Cardinal);
    procedure WriteBrushPropList(Level: Integer; nIndex: Cardinal);
    procedure WritePropData(Level: Integer; pObject: TLTWorldObjectProperty);
    procedure WriteNavigatorPosList(Level: Integer);

    // helpers
    function GetLevel(Level: Integer): string;
    function GetExtData({%H-}pProperty: TLTWorldObjectProperty): string;
    procedure BuildClassList(nLength: Cardinal; pObjectList: TFPObjectList);
    procedure BuildSimpleBrushObject(pObject: TLTWorldBsp);
    procedure BuildPolyBrushObject(pObject: TLTWorldBsp);
    function FindBrushPropId(szName: string): Cardinal;
    procedure CheckModelSurfaces(pBSP: TLTWorldBsp);
  public
    procedure ExportText(S: string);
    constructor Create(pReader: TLTWorldReader);
    destructor Destroy; override;
  end;


implementation

function TLTAWorldExporter.GetLevel(Level: Integer): string;
var i: Integer;
begin
  Result := '';
  for i := 1 to Level do
  begin
    Result := Result + '	';
  end;
end;

function TLTAWorldExporter.GetExtData(pProperty: TLTWorldObjectProperty): string;
begin
  Result := '';
  { TODO : groups and other shit }
end;

procedure TLTAWorldExporter.BuildClassList(nLength: Cardinal; pObjectList: TFPObjectList);
var i: Cardinal;
    pObject: TLTWorldObject;
begin
  if nLength > 0 then
  for i := 0 to nLength - 1 do
  begin
    pObject := TLTWorldObject(pObjectList.Items[i]);
    if m_slClassList.IndexOf(pObject.TypeString) = -1 then
    begin
      m_slClassList.Add(pObject.TypeString);
    end
  end;
  // world geometry classes
  m_slClassList.Add(m_szMainGeometrySource);
  m_slClassList.SaveToFile(CPData.DumpsDir + CPData.Sep + 'ClassList.txt');
end;

procedure TLTAWorldExporter.BuildSimpleBrushObject(pObject: TLTWorldBsp);
var pBrush: TLTWorldBrush;
    szName, szType, szLighting, szTextureEffect: string;
    vPos, vAmbientLight: LTVector;
    rRot: LTRotation;
    dwRenderGroup, dwLightControl, dwLMGridSize: Cardinal;
    fLightPenScale, fCreaseAngle: LTFloat;
    nNotAStep, nDetail, nClipLight, nCastShadowMesh, nReceiveLight,
    nReceiveShadows, nReceiveSunlight: Byte;

    pPoly: TLTWorldPoly;
    pSurface: TLTWorldSurface;

begin
  // check surfaces first
  //CheckModelSurfaces(pObject);
  pBrush := TLTWorldBrush.Create;
  pBrush.BspName := pObject.WorldName;
  {		( proplist (
			(  string "Name" (  ) ( data "Brush4") )
			(  vector "Pos" ( distance  ) ( data ( vector (0.000000 0.000000 0.000000) ) ) )
			(  rotation "Rotation" (  ) ( data (eulerangles (0.000000 0.000000 0.000000) ) ) )
			(  longint "RenderGroup" (  ) ( data 0.000000 ))
			(  string "Type" ( staticlist  ) ( data "Normal") )
			(  string "Lighting" ( staticlist  ) ( data "Gouraud") )
			(  bool "NotAStep" (  ) ( data 0 ))
			(  bool "Detail" (  ) ( data 1 ))
			(  longint "LightControl" ( groupowner group1  ) ( data 0.000000 ))
			(  string "TextureEffect" ( textureeffect ) )
			(  color "AmbientLight" ( group1  ) ( data ( vector (0.000000 0.000000 0.000000) ) ) )
			(  longint "LMGridSize" ( group1  ) ( data 0.000000 ))
			(  bool "ClipLight" ( group1  ) ( data 1 ))
			(  bool "CastShadowMesh" ( group1  ) ( data 1 ))
			(  bool "ReceiveLight" ( group1  ) ( data 1 ))
			(  bool "ReceiveShadows" ( group1  ) ( data 1 ))
			(  bool "ReceiveSunlight" ( group1  ) ( data 1 ))
			(  real "LightPenScale" ( group1  ) ( data 0.000000 ))
			(  real "CreaseAngle" ( group1  ) ( data 45.000000 ))
		) )}

  if m_nBrushType = BT_NOLF2 then
  begin
    szName := 'Brush_' + pObject.WorldName;
    vPos := LTVectorInit(0, 0, 0);
    rRot := LTRotationInit(0, 0, 0, 0);
    dwRenderGroup := 0;
    szType := 'Normal';
    szLighting := 'Gouraud';
    nNotAStep := 0;
    nDetail := 1;
    dwLightControl := 0;
    szTextureEffect := '';
    vAmbientLight := LTVectorInit(0, 0, 0);
    dwLMGridSize := 0;
    nClipLight := 1;
    nCastShadowMesh := 1;
    nReceiveLight := 1;
    nReceiveShadows := 1;
    nReceiveSunlight := 1;
    fLightPenScale := 0;
    fCreaseAngle := 45;

    pBrush.CreateProperty(PT_STRING, 'Name', @szName);
    pBrush.CreateProperty(PT_VECTOR, 'Pos', @vPos);
    pBrush.CreateProperty(PT_ROTATION, 'Rotation', @rRot);
    pBrush.CreateProperty(PT_LONGINT, 'RenderGroup', @dwRenderGroup);
    pBrush.CreateProperty(PT_STRING, 'Type', @szType);
    pBrush.CreateProperty(PT_STRING, 'Lighting', @szLighting);
    pBrush.CreateProperty(PT_BOOL, 'NotAStep', @nNotAStep);
    pBrush.CreateProperty(PT_BOOL, 'Detail', @nDetail);
    pBrush.CreateProperty(PT_LONGINT, 'LightControl', @dwLightControl);
    pBrush.CreateProperty(PT_STRING, 'TextureEffect', @szTextureEffect);
    pBrush.CreateProperty(PT_COLOR, 'AmbientLight', @vAmbientLight);
    pBrush.CreateProperty(PT_LONGINT, 'LMGridSize', @dwLMGridSize);
    pBrush.CreateProperty(PT_BOOL, 'ClipLight', @nClipLight);
    pBrush.CreateProperty(PT_BOOL, 'CastShadowMesh', @nCastShadowMesh);
    pBrush.CreateProperty(PT_BOOL, 'ReceiveLight', @nReceiveLight);
    pBrush.CreateProperty(PT_BOOL, 'ReceiveShadows', @nReceiveShadows);
    pBrush.CreateProperty(PT_BOOL, 'ReceiveSunlight', @nReceiveSunlight);
    pBrush.CreateProperty(PT_REAL, 'LightPenScale', @fLightPenScale);
    pBrush.CreateProperty(PT_REAL, 'CreaseAngle', @fCreaseAngle);
  end
  else
  if m_nBrushType = BT_AVP2 then
  begin
    pPoly := TLTWorldPoly(pObject.PoliesList.Items[0]);
    pSurface := TLTWorldSurface(pObject.SurfacesList.Items[pPoly.Surface]);
    //WriteLn(binStr(pObject.WorldInfoFlags, 16));

    pBrush.WritePropString('Name', 'Brush_' + pObject.WorldName);
    pBrush.WritePropVector('Pos', LTVectorInit(0, 0, 0));
    pBrush.WritePropRotation('Rotation', LTRotationInit(0, 0, 0, 0));

    pBrush.WritePropBool('Solid', FlagsToBool(pSurface.m_nFlags, SURF_SOLID));
    pBrush.WritePropBool('Nonexistant', FlagsToBool(pSurface.m_nFlags, SURF_NONEXISTANT));
    pBrush.WritePropBool('Invisible', FlagsToBool(pSurface.m_nFlags, SURF_INVISIBLE));
    pBrush.WritePropBool('Translucent', FlagsToBool(pSurface.m_nFlags, SURF_TRANSPARENT));
    pBrush.WritePropBool('SkyPortal', FlagsToBool(pSurface.m_nFlags, SURF_SKY));
    pBrush.WritePropBool('FullyBright', FlagsToBool(pSurface.m_nFlags, SURF_BRIGHT));
    pBrush.WritePropBool('FlatShade', FlagsToBool(pSurface.m_nFlags, SURF_FLATSHADE));
    pBrush.WritePropBool('GouraudShade', FlagsToBool(pSurface.m_nFlags, SURF_GOURAUDSHADE));
    pBrush.WritePropBool('LightMap', FlagsToBool(pSurface.m_nFlags, SURF_LIGHTMAP));
    pBrush.WritePropBool('Subdivide', FlagsToBool(pSurface.m_nFlags, SURF_NOSUBDIV, True));
    pBrush.WritePropBool('HullMaker', FlagsToBool(pSurface.m_nFlags, SURF_HULLMAKER));
    pBrush.WritePropBool('AlwaysLightMap', FlagsToBool(pSurface.m_nFlags, SURF_ALWAYSLIGHTMAP));
    pBrush.WritePropBool('DirectionalLight', FlagsToBool(pSurface.m_nFlags, SURF_DIRECTIONALLIGHT));
    pBrush.WritePropBool('Portal', FlagsToBool(pSurface.m_nFlags, SURF_PORTAL));
    pBrush.WritePropBool('NoSnap', 1);
    pBrush.WritePropBool('SkyPan', FlagsToBool(pSurface.m_nFlags, SURF_PANNINGSKY));
    pBrush.WritePropBool('Additive', FlagsToBool(pSurface.m_nFlags, SURF_ADDITIVE));
    pBrush.WritePropBool('TerrainOccluder', FlagsToBool(pSurface.m_nFlags, SURF_TERRAINOCCLUDER));
    pBrush.WritePropBool('TimeOfDay', FlagsToBool(pSurface.m_nFlags, SURF_TIMEOFDAY));
    pBrush.WritePropBool('VisBlocker', FlagsToBool(pSurface.m_nFlags, SURF_VISBLOCKER));
    pBrush.WritePropBool('NotAStep', FlagsToBool(pSurface.m_nFlags, SURF_NOTASTEP));
    pBrush.WritePropBool('NoWallWalk', FlagsToBool(pSurface.m_nFlags, SURF_NOWALLWALK));
    pBrush.WritePropBool('BlockLight', FlagsToBool(pSurface.m_nFlags, SURF_NOBLOCKLIGHT, True));

    pBrush.WritePropLongInt('DetailLevel', 0);
    pBrush.WritePropString('Effect', pSurface.m_szEffect);
    pBrush.WritePropString('EffectParam', pSurface.m_szEffectParam);
    pBrush.WritePropReal('FrictionCoefficient', 1);

  end;

  m_pBrushPropList.Add(pBrush);
end;

procedure TLTAWorldExporter.BuildPolyBrushObject(pObject: TLTWorldBsp);
var i: Cardinal;
    pPoly: TLTWorldPoly;
    pSurface: TLTWorldSurface;
    pBrush: TLTWorldBrush;

    szName, szType, szLighting, szTextureEffect: string;
    vPos, vAmbientLight: LTVector;
    rRot: LTRotation;
    dwRenderGroup, dwLightControl, dwLMGridSize: Cardinal;
    fLightPenScale, fCreaseAngle: LTFloat;
    nNotAStep, nDetail, nClipLight, nCastShadowMesh, nReceiveLight,
    nReceiveShadows, nReceiveSunlight: Byte;
begin
  if pObject.Polies > 0 then
  begin
    if m_nBrushType = BT_AVP2 then
    begin
      for i := 0 to pObject.Polies - 1 do
      begin
        pBrush := TLTWorldBrush.Create;
        pBrush.BspName := pObject.WorldName + IntToStr(i);
        //m_slModelList.Add(pBrush.BspName);

        pPoly := TLTWorldPoly(pObject.PoliesList.Items[i]);
        pSurface := TLTWorldSurface(pObject.SurfacesList.Items[pPoly.Surface]);

        pBrush.WritePropString('Name', 'Brush_' + pBrush.BspName);
        pBrush.WritePropVector('Pos', LTVectorInit(0, 0, 0));
        pBrush.WritePropRotation('Rotation', LTRotationInit(0, 0, 0, 0));

        pBrush.WritePropBool('Solid', FlagsToBool(pSurface.m_nFlags, SURF_SOLID));
        pBrush.WritePropBool('Nonexistant', FlagsToBool(pSurface.m_nFlags, SURF_NONEXISTANT));
        pBrush.WritePropBool('Invisible', FlagsToBool(pSurface.m_nFlags, SURF_INVISIBLE));
        pBrush.WritePropBool('Translucent', FlagsToBool(pSurface.m_nFlags, SURF_TRANSPARENT));
        pBrush.WritePropBool('SkyPortal', FlagsToBool(pSurface.m_nFlags, SURF_SKY));
        pBrush.WritePropBool('FullyBright', FlagsToBool(pSurface.m_nFlags, SURF_BRIGHT));
        pBrush.WritePropBool('FlatShade', FlagsToBool(pSurface.m_nFlags, SURF_FLATSHADE));
        pBrush.WritePropBool('GouraudShade', FlagsToBool(pSurface.m_nFlags, SURF_GOURAUDSHADE));
        pBrush.WritePropBool('LightMap', FlagsToBool(pSurface.m_nFlags, SURF_LIGHTMAP));
        pBrush.WritePropBool('Subdivide', FlagsToBool(pSurface.m_nFlags, SURF_NOSUBDIV, True));
        pBrush.WritePropBool('HullMaker', FlagsToBool(pSurface.m_nFlags, SURF_HULLMAKER));
        pBrush.WritePropBool('AlwaysLightMap', FlagsToBool(pSurface.m_nFlags, SURF_ALWAYSLIGHTMAP));
        pBrush.WritePropBool('DirectionalLight', FlagsToBool(pSurface.m_nFlags, SURF_DIRECTIONALLIGHT));
        pBrush.WritePropBool('Portal', FlagsToBool(pSurface.m_nFlags, SURF_PORTAL));
        pBrush.WritePropBool('NoSnap', 1);
        pBrush.WritePropBool('SkyPan', FlagsToBool(pSurface.m_nFlags, SURF_PANNINGSKY));
        pBrush.WritePropBool('Additive', FlagsToBool(pSurface.m_nFlags, SURF_ADDITIVE));
        pBrush.WritePropBool('TerrainOccluder', FlagsToBool(pSurface.m_nFlags, SURF_TERRAINOCCLUDER));
        pBrush.WritePropBool('TimeOfDay', FlagsToBool(pSurface.m_nFlags, SURF_TIMEOFDAY));
        pBrush.WritePropBool('VisBlocker', FlagsToBool(pSurface.m_nFlags, SURF_VISBLOCKER));
        pBrush.WritePropBool('NotAStep', FlagsToBool(pSurface.m_nFlags, SURF_NOTASTEP));
        pBrush.WritePropBool('NoWallWalk', FlagsToBool(pSurface.m_nFlags, SURF_NOWALLWALK));
        pBrush.WritePropBool('BlockLight', FlagsToBool(pSurface.m_nFlags, SURF_NOBLOCKLIGHT, True));

        // TEST IT!
        pBrush.WritePropLongInt('DetailLevel', FlagsToBool(pSurface.m_nFlags, SURF_INVISIBLE));
        pBrush.WritePropString('Effect', pSurface.m_szEffect);
        pBrush.WritePropString('EffectParam', pSurface.m_szEffectParam);
        pBrush.WritePropReal('FrictionCoefficient', 1);


        m_pBrushPropList.Add(pBrush);
      end;
    end
    else if m_nBrushType = BT_NOLF2 then
    begin
      szName := 'Brush_' + pObject.WorldName;
      vPos := LTVectorInit(0, 0, 0);
      rRot := LTRotationInit(0, 0, 0, 0);
      dwRenderGroup := 0;
      szType := 'Normal';
      szLighting := 'Gouraud';
      nNotAStep := 0;
      nDetail := 1;
      dwLightControl := 0;
      szTextureEffect := '';
      vAmbientLight := LTVectorInit(0, 0, 0);
      dwLMGridSize := 0;
      nClipLight := 1;
      nCastShadowMesh := 1;
      nReceiveLight := 1;
      nReceiveShadows := 1;
      nReceiveSunlight := 1;
      fLightPenScale := 0;
      fCreaseAngle := 45;

      for i := 0 to pObject.Polies - 1 do
      begin
        pBrush := TLTWorldBrush.Create;
        pBrush.BspName := pObject.WorldName + IntToStr(i);
        pBrush.CreateProperty(PT_STRING, 'Name', @szName);
        pBrush.CreateProperty(PT_VECTOR, 'Pos', @vPos);
        pBrush.CreateProperty(PT_ROTATION, 'Rotation', @rRot);
        pBrush.CreateProperty(PT_LONGINT, 'RenderGroup', @dwRenderGroup);
        pBrush.CreateProperty(PT_STRING, 'Type', @szType);
        pBrush.CreateProperty(PT_STRING, 'Lighting', @szLighting);
        pBrush.CreateProperty(PT_BOOL, 'NotAStep', @nNotAStep);
        pBrush.CreateProperty(PT_BOOL, 'Detail', @nDetail);
        pBrush.CreateProperty(PT_LONGINT, 'LightControl', @dwLightControl);
        pBrush.CreateProperty(PT_STRING, 'TextureEffect', @szTextureEffect);
        pBrush.CreateProperty(PT_COLOR, 'AmbientLight', @vAmbientLight);
        pBrush.CreateProperty(PT_LONGINT, 'LMGridSize', @dwLMGridSize);
        pBrush.CreateProperty(PT_BOOL, 'ClipLight', @nClipLight);
        pBrush.CreateProperty(PT_BOOL, 'CastShadowMesh', @nCastShadowMesh);
        pBrush.CreateProperty(PT_BOOL, 'ReceiveLight', @nReceiveLight);
        pBrush.CreateProperty(PT_BOOL, 'ReceiveShadows', @nReceiveShadows);
        pBrush.CreateProperty(PT_BOOL, 'ReceiveSunlight', @nReceiveSunlight);
        pBrush.CreateProperty(PT_REAL, 'LightPenScale', @fLightPenScale);
        pBrush.CreateProperty(PT_REAL, 'CreaseAngle', @fCreaseAngle);
      end;
    end;


  end;
end;

function TLTAWorldExporter.FindBrushPropId(szName: string): Cardinal;
var i: Cardinal;
begin
  if m_pBrushPropList.Count > 0 then
  begin
    for i := 0 to m_pBrushPropList.Count - 1 do
    begin
      if TLTWorldBrush(m_pBrushPropList.Items[i]).BspName = szName then Exit(i);
    end;
  end;
end;

procedure TLTAWorldExporter.CheckModelSurfaces(pBSP: TLTWorldBsp);
var i: Cardinal;
    pPoly: TLTWorldPoly;
    pSurface: TLTWorldSurface;
    nFlagsTemp: Cardinal;
    //nUnknownTemp: Cardinal;
begin
  if pBSP.Polies > 0 then
  for i := 0 to pBSP.Polies - 1 do
  begin
    pPoly := TLTWorldPoly(pBSP.PoliesList.Items[i]);
    pSurface := TLTWorldSurface(pBSP.SurfacesList.Items[pPoly.Surface]);
    if i = 0 then
    begin
      nFlagsTemp := pSurface.m_nFlags;
      //nUnknownTemp := pSurface.m_nUnknown1;
    end
    else
    begin
      if (nFlagsTemp <> pSurface.m_nFlags) then
      //or (nUnknownTemp <> pSurface.m_nUnknown1) then
      begin
        Logger.WLog(LM_WARN, 'WorldModel "' + pBSP.WorldName + '" has different surface flags!');
        Exit;
      end;
    end;
end;
end;

constructor TLTAWorldExporter.Create(pReader: TLTWorldReader);
begin
  if g_szBrushType = 'avp2' then m_nBrushType := BT_AVP2
  else if g_szBrushType = 'nolf2' then m_nBrushType := BT_NOLF2;
  if g_szBrushGenType = 'simple' then m_nBrushGetType := BGT_SIMPLE
  else if g_szBrushGenType = 'poly' then m_nBrushGetType := BGT_POLY;

  m_pReader := pReader;

  if g_szGeometrySource = 'physics' then
  begin
    m_szMainGeometrySource := BSP_PHYSICS;
    m_pReader.RemoveWorldModel(BSP_VIS);
  end
  else if g_szGeometrySource = 'vis' then
  begin
    m_szMainGeometrySource := BSP_VIS;
    m_pReader.RemoveWorldModel(BSP_PHYSICS);
  end;

  m_pReader.MoveWorldModel(m_szMainGeometrySource, m_pReader.ModelList.pModelList.Count - 1);

  //m_pMyCP := pCP;
  //m_pLogger := pLogger;
  m_slExport := TStringList.Create;
  m_slClassList := TStringList.Create;
  m_slModelList := TStringList.Create;
  m_pBrushPropList := TFPObjectList.Create(True);
end;

destructor TLTAWorldExporter.Destroy;
begin
  inherited;
  m_pBrushPropList.Free;
  m_slClassList.Free;
  m_slModelList.Free;
  m_slExport.Free;
end;

procedure TLTAWorldExporter.ExportText(S: string);
begin
  WriteLTWorld;
  m_slExport.SaveToFile(S);
end;

procedure TLTAWorldExporter.WriteNodeStart(Level: Integer; ID: string; Name: string);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID + Name + '(');
end;

procedure TLTAWorldExporter.WriteNodeEnd(Level: Integer);
begin
  m_slExport.Add(GetLevel(Level) +  ') )');
end;

procedure TLTAWorldExporter.WriteArrayStart(Level: Integer; ID: string; Name: string);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID + Name);
end;

procedure TLTAWorldExporter.WriteArrayEnd(Level: Integer);
begin
  m_slExport.Add(GetLevel(Level) +  ')');
end;

procedure TLTAWorldExporter.WriteGenericSet(Level: Integer; Args: array of string);
var i: Integer;
    szBuffer: string;
begin
  szBuffer := '';
  for i := 0 to Length(Args) - 1 do
  begin
    szBuffer := szBuffer + Args[i] + ' ';
  end;
  m_slExport.Add(GetLevel(Level) + '(' + szBuffer + ')');
end;

procedure TLTAWorldExporter.WriteGenericProp(Level: Integer; ID: string; Value: string);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID + Value + ' )');
end;

procedure TLTAWorldExporter.WriteGenericPropStr(Level: Integer; ID: string; Value: string);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID + '"' + Value + '" )');
end;

procedure TLTAWorldExporter.WriteGenericPropExt(Level: Integer; DataType: string; ID: string; ExtData: string; Value: string);
begin
  m_slExport.Add(GetLevel(Level) + '( ' + DataType + '"' + ID + '" ' +
  '( ' + ExtData + ' ) ' + '( data ' + Value + ' ) )');
end;

procedure TLTAWorldExporter.WriteArrayVector(Level: Integer; ID: string; Vector: PLTVector);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID);
  m_slExport.Add(GetLevel(Level + 1) + '(' + LTVectorToStrC(Vector) + ' )');
  m_slExport.Add(GetLevel(Level) + ')');
end;

procedure TLTAWorldExporter.WriteVector(Level: Integer; Vector: PLTVector);
begin
  m_slExport.Add(GetLevel(Level) + '( ' + LTVectorToStrC(Vector) + ' )');
end;

procedure TLTAWorldExporter.WriteWorldPoint(Level: Integer; Vector: PLTVector);
begin
  m_slExport.Add(GetLevel(Level) + '( ' + LTVectorToStrC(Vector) + ' 255 255 255 255 )');
end;

procedure TLTAWorldExporter.WriteArrayRotation(Level: Integer; ID: string; Rotation: PLTRotation);
begin
  m_slExport.Add(GetLevel(Level) + '(' + ID);
  m_slExport.Add(GetLevel(Level + 1) + '(' + LTRotationToStrC(Rotation) + ' )');
  m_slExport.Add(GetLevel(Level) + ')');
end;

procedure TLTAWorldExporter.WriteRotation(Level: Integer; Rotation: PLTRotation);
begin
  m_slExport.Add(GetLevel(Level) + '(' + LTRotationToStrC(Rotation) + ' )');
end;

procedure TLTAWorldExporter.WriteLineComment(Level: Integer; Text: string);
begin
  m_slExport.Add(GetLevel(Level) + '// ' + Text);
end;

procedure TLTAWorldExporter.WriteHeader(Level: Integer);
begin
  WriteNodeStart(Level, NODE_HEADER, '');
  WriteGenericProp(Level + 1, PROP_VERSIONCODE, '2');
  WriteGenericPropStr(Level + 1, PROP_INFOSTRING, m_pReader.Properties);
  WriteNodeEnd(Level);
end;

procedure TLTAWorldExporter.WritePolyhedronList(Level: Integer);
var pModel: TLTWorldBsp;
    pPoly: TLTWorldPoly;
    pPlane: TLTWorldPlane;
    pSurface: TLTWorldSurface;
    i, j, k, n: Cardinal;
    szPointsList: string;
    vNull: LTVector;
    bSepBsp: Boolean;
begin
  WriteNodeStart(Level, NODE_POLYHEDRONLIST, '');

  for i := 0 to m_pReader.ModelList.nNumModels - 1 do
  begin
    bSepBsp := False;
    pModel := TLTWorldData(m_pReader.ModelList.pModelList.Items[i]).OriginalBSP;

    //if (pModel.WorldName = BSP_PHYSICS) then
    //  Sleep(0);

    //if (pModel.WorldName <> 'VisBSP') then //and (pModel.WorldName <> 'PhysicsBSP') then
    //if WorldModelFilter(pModel.WorldName) then
    begin
      m_slModelList.Add(pModel.WorldName);

      // construct brush props
      if m_nBrushGetType = BGT_POLY then
      begin
        if pModel.WorldName = m_szMainGeometrySource then
        begin
          BuildPolyBrushObject(pModel);
          bSepBsp := True;
        end
        else
        begin
          BuildSimpleBrushObject(pModel);
        end;
      end
      else if m_nBrushGetType = BGT_SIMPLE then
      begin
        BuildSimpleBrushObject(pModel);
      end;

      if not bSepBsp then
      begin
        WriteNodeStart(Level + 1, NODE_POLYHEDRON, '');

        // debug START
        if g_bDebugProps = True then
          WriteLineComment(Level + 2, pModel.WorldName + ' brush #' + IntToStr(i));
        // debug END

        WriteGenericSet(Level + 2, [ PROP_COLOR2, '255', '255', '255']);
        if pModel.Points > 0 then
        begin
          WriteArrayStart(Level + 2, ARRAY_POINTLIST, '');
          for j := 0 to pModel.Points - 1 do
          begin
            WriteWorldPoint(Level + 3, @TLTWorldVertex(pModel.PointsList.Items[j]).m_vData);
          end;
          WriteArrayEnd(Level + 2);
        end;
        WriteNodeStart(Level + 2, NODE_POLYLIST, '');
        if pModel.Polies > 0 then
        begin
          for j := 0 to pModel.Polies - 1 do
          begin
            WriteArrayStart(Level + 3, ARRAY_EDITPOLY, '');
            pPoly := TLTWorldPoly(pModel.PoliesList.Items[j]);

            szPointsList := '';
            if pPoly.GetNumVertices > 0 then
            // SURF_PHYSICSFIX polies?
            //for k := 0 to pPoly.GetNumVertices - 1 do
            for k := 0 to pPoly.LoVerts - 1 do
            begin
              szPointsList := szPointsList + ' ' + IntToStr(pPoly.DiskVerts[k].nVerts);
            end;
            WriteGenericProp(Level + 4, PROP_F, szPointsList);

            pPlane := TLTWorldPlane(pModel.PlanesList.Items[pPoly.Plane]);
            WriteGenericProp(Level + 4, PROP_N, LTVectorToStrC(@pPlane.m_vNormal));
            WriteGenericProp(Level + 4, PROP_DIST, FormatFloat('0.000000', pPlane.m_fDist));

            WriteArrayStart(Level + 4, ARRAY_TEXTUREINFO, '');
            WriteVector(Level + 5, @pPoly.UVData1);
            WriteVector(Level + 5, @pPoly.UVData2);
            WriteVector(Level + 5, @pPoly.UVData3);
            WriteGenericProp(Level + 5, PROP_STICKTOPOLY, '1');
            pSurface := TLTWorldSurface(pModel.SurfacesList.Items[pPoly.Surface]);
            WriteGenericPropStr(Level + 5, PROP_NAME, pModel.TextureNames[pSurface.m_nTexture]);
            WriteArrayEnd(Level + 4);

            WriteGenericSet(Level + 4, [PROP_FLAGS]);
            WriteGenericSet(Level + 4, [PROP_SHADE, '0', '0', '0']);
            WriteGenericPropStr(Level + 4, PROP_PHYSICSMATERIAL, 'Default');
            WriteGenericPropStr(Level + 4, PROP_SURFACEKEY, '');

            WriteNodeStart(Level + 4, NODE_TEXTURES, '');
            WriteArrayStart(Level + 5, ' 1 ', '');
            WriteArrayStart(Level + 6, ARRAY_TEXTUREINFO, '');
            vNull := LTVectorInit(0, 0, 0);
            WriteVector(Level + 7, @vNull);
            WriteVector(Level + 7, @pPoly.UVData2);
            WriteVector(Level + 7, @pPoly.UVData3);
            WriteGenericProp(Level + 7, PROP_STICKTOPOLY, '1');
            WriteGenericPropStr(Level + 7, PROP_NAME, 'Default');
            WriteArrayEnd(Level + 6);
            WriteArrayEnd(Level + 5);
            WriteNodeEnd(Level + 4);

            WriteArrayEnd(Level + 3);
          end;
        end;
        WriteNodeEnd(Level + 2);
        WriteNodeEnd(Level + 1);
      end
      else
      begin
        for n := 0 to pModel.Polies - 1 do
        begin
          WriteNodeStart(Level + 1, NODE_POLYHEDRON, '');

          // debuf START
          if g_bDebugProps = True then
            WriteLineComment(Level + 2, pModel.WorldName + IntToStr(n) + ' brush #' + {%H-}IntToStr(i + n));
          // debug END

          WriteGenericSet(Level + 2, [ PROP_COLOR2, '255', '255', '255']);
          pPoly := TLTWorldPoly(pModel.PoliesList.Items[n]);
          if pPoly.LoVerts > 0 then
          begin
            WriteArrayStart(Level + 2, ARRAY_POINTLIST, '');
            for j := 0 to pPoly.LoVerts - 1 do
            begin
              WriteWorldPoint(Level + 3, @TLTWorldVertex(pModel.PointsList.Items[pPoly.DiskVerts[j].nVerts]).m_vData);
            end;
            WriteArrayEnd(Level + 2);
          end;
          WriteNodeStart(Level + 2, NODE_POLYLIST, '');

          WriteArrayStart(Level + 3, ARRAY_EDITPOLY, '');

          szPointsList := '';
          if pPoly.GetNumVertices > 0 then
          // SURF_PHYSICSFIX polies?
          //for k := 0 to pPoly.GetNumVertices - 1 do
          for k := 0 to pPoly.LoVerts - 1 do
          begin
            szPointsList := szPointsList + ' ' + IntToStr(k);
          end;
          WriteGenericProp(Level + 4, PROP_F, szPointsList);

          pPlane := TLTWorldPlane(pModel.PlanesList.Items[pPoly.Plane]);
          WriteGenericProp(Level + 4, PROP_N, LTVectorToStrC(@pPlane.m_vNormal));
          WriteGenericProp(Level + 4, PROP_DIST, FormatFloat('0.000000', pPlane.m_fDist));

          WriteArrayStart(Level + 4, ARRAY_TEXTUREINFO, '');
          WriteVector(Level + 5, @pPoly.UVData1);
          WriteVector(Level + 5, @pPoly.UVData2);
          WriteVector(Level + 5, @pPoly.UVData3);
          WriteGenericProp(Level + 5, PROP_STICKTOPOLY, '1');
          pSurface := TLTWorldSurface(pModel.SurfacesList.Items[pPoly.Surface]);
          WriteGenericPropStr(Level + 5, PROP_NAME, pModel.TextureNames[pSurface.m_nTexture]);
          WriteArrayEnd(Level + 4);

          WriteGenericSet(Level + 4, [PROP_FLAGS]);
          WriteGenericSet(Level + 4, [PROP_SHADE, '0', '0', '0']);
          WriteGenericPropStr(Level + 4, PROP_PHYSICSMATERIAL, 'Default');
          WriteGenericPropStr(Level + 4, PROP_SURFACEKEY, '');

          WriteNodeStart(Level + 4, NODE_TEXTURES, '');
          WriteArrayStart(Level + 5, ' 1 ', '');
          WriteArrayStart(Level + 6, ARRAY_TEXTUREINFO, '');
          vNull := LTVectorInit(0, 0, 0);
          WriteVector(Level + 7, @vNull);
          WriteVector(Level + 7, @pPoly.UVData2);
          WriteVector(Level + 7, @pPoly.UVData3);
          WriteGenericProp(Level + 7, PROP_STICKTOPOLY, '1');
          WriteGenericPropStr(Level + 7, PROP_NAME, 'Default');
          WriteArrayEnd(Level + 6);
          WriteArrayEnd(Level + 5);
          WriteNodeEnd(Level + 4);


          WriteArrayEnd(Level + 3);


          WriteNodeEnd(Level + 2);
          WriteNodeEnd(Level + 1);
        end;
      end;
    end;
  end;
  WriteNodeEnd(Level);
  m_slModelList.SaveToFile(CPData.DumpsDir + CPData.Sep + 'ModelList.txt');
end;

procedure TLTAWorldExporter.WriteNodeHierarchy(Level: Integer);
var i, j, k: Integer;
    nNodeCounter: Cardinal;
    pObject: TLTWorldObject;
    pBSP: TLTWorldBsp;
begin
  WriteArrayStart(Level, ARRAY_HIERARCHY, '');

  WriteArrayStart(Level + 1, ARRAY_WORLDNODE, '');
  WriteGenericProp(Level + 2, PROP_TYPE, 'null');
  WriteGenericPropStr(Level + 2, PROP_LABEL, 'WorldRoot');
  WriteGenericProp(Level + 2, PROP_NODEID, '1');
  WriteGenericProp(Level + 2, PROP_FLAGS, '( worldroot expanded )');
  WriteArrayStart(Level + 2, ARRAY_PROPERTIES, '');
  WriteGenericProp(Level + 3, PROP_PROPID, '0');
  WriteArrayEnd(Level + 2);
  WriteNodeStart(Level + 2, NODE_CHILDLIST, '');

  BuildClassList(m_pReader.ObjectList.nNumObjects, m_pReader.ObjectList.pObjectList);

  nNodeCounter := 2;
  k := 0;
  for i := 0 to m_slClassList.Count - 1 do
  //for i := m_slClassList.Count - 1 downto 0 do
  begin
    // ignore objects
    {if m_bIgnoreObjects and
       (m_slClassList.Strings[i] <> 'PhysicsBSP') and
       (Pos('WorldModel', m_slClassList.Strings[i]) = 0) then
    begin
      WriteLn('--- Skipping node: ', m_slClassList.Strings[i]);
      Continue;
    end;   }

    WriteArrayStart(Level + 3, ARRAY_WORLDNODE, '');
    WriteGenericProp(Level + 4, PROP_TYPE, 'null');
    WriteGenericPropStr(Level + 4, PROP_LABEL, m_slClassList.Strings[i] + 'Group');
    WriteGenericProp(Level + 4, PROP_NODEID, IntToStr(nNodeCounter));
    WriteGenericProp(Level + 4, PROP_FLAGS, '( )');
    WriteArrayStart(Level + 4, ARRAY_PROPERTIES, '');
    WriteGenericProp(Level + 5, PROP_PROPID, '0');
    WriteArrayEnd(Level + 4);
    WriteNodeStart(Level + 4, NODE_CHILDLIST, '');
    //Inc(nNodeCounter, 1);

    if m_slClassList.Strings[i] = m_szMainGeometrySource then
    begin
      if m_nBrushGetType = BGT_SIMPLE then
      begin
        Inc(nNodeCounter, 1);
        WriteArrayStart(Level + 5, ARRAY_WORLDNODE, '');
        WriteGenericProp(Level + 6, PROP_TYPE, 'brush');
        WriteGenericProp(Level + 6, PROP_BRUSHINDEX, IntToStr(m_slModelList.Count - 1));
        WriteGenericProp(Level + 6, PROP_NODEID, IntToStr(nNodeCounter));
        WriteGenericProp(Level + 6, PROP_FLAGS, '( )');
        WriteArrayStart(Level + 6, ARRAY_PROPERTIES, '');
        WriteGenericPropStr(Level + 7, PROP_NAME, 'Brush');
        WriteGenericProp(Level + 7, PROP_PROPID, {%H-}IntToStr(m_pReader.ObjectList.nNumObjects + Cardinal(m_slModelList.Count)));
        //WriteGenericProp(Level + 7, PROP_PROPID, IntToStr(nNodeCounter));
        WriteArrayEnd(Level + 6);
        WriteArrayEnd(Level + 5);
      end
      else if m_nBrushGetType = BGT_POLY then
      begin
        pBSP := TLTWorldData(m_pReader.ModelList.pModelList.Items[m_pReader.ModelList.nNumModels - 1]).OriginalBSP;
        for j := 0 to pBSP.Polies - 1 do
        begin
          Inc(nNodeCounter, 1);
          WriteArrayStart(Level + 5, ARRAY_WORLDNODE, '');
          WriteGenericProp(Level + 6, PROP_TYPE, 'brush');
          WriteGenericProp(Level + 6, PROP_BRUSHINDEX, IntToStr(m_slModelList.Count - 1 + j));
          WriteGenericProp(Level + 6, PROP_NODEID, IntToStr(nNodeCounter));
          WriteGenericProp(Level + 6, PROP_FLAGS, '( )');
          WriteArrayStart(Level + 6, ARRAY_PROPERTIES, '');
          WriteGenericPropStr(Level + 7, PROP_NAME, 'Brush');
          WriteGenericProp(Level + 7, PROP_PROPID, {%H-}IntToStr(m_pReader.ObjectList.nNumObjects + Cardinal(m_slModelList.Count + j)));
          //WriteGenericProp(Level + 7, PROP_PROPID, IntToStr(nNodeCounter));
          WriteArrayEnd(Level + 6);
          WriteArrayEnd(Level + 5);
        end;
      end;
    end
    else
    begin
      for j := 0 to m_pReader.ObjectList.nNumObjects - 1 do
      begin
        pObject := TLTWorldObject(m_pReader.ObjectList.pObjectList.Items[j]);
        if pObject.TypeString = m_slClassList.Strings[i] then
        begin
          // debug START
          if g_bDebugProps then
            WriteLineComment(Level + 5, pObject.GetObjectName);
          // debug END

          Inc(nNodeCounter, 1);
          WriteArrayStart(Level + 5, ARRAY_WORLDNODE, '');
          WriteGenericProp(Level + 6, PROP_TYPE, 'object');

          WriteGenericProp(Level + 6, PROP_NODEID, IntToStr(nNodeCounter));
          WriteGenericProp(Level + 6, PROP_FLAGS, '( )');
          WriteArrayStart(Level + 6, ARRAY_PROPERTIES, '');
          WriteGenericPropStr(Level + 7, PROP_NAME, pObject.TypeString);
          WriteGenericProp(Level + 7, PROP_PROPID, IntToStr(j + 1));
          WriteArrayEnd(Level + 6);

          // associated brush
          k := m_slModelList.IndexOf(pObject.GetObjectName);
          if k > - 1 then
          begin
            Inc(nNodeCounter, 1);
            WriteNodeStart(Level + 6, NODE_CHILDLIST, '');
            WriteArrayStart(Level + 7, ARRAY_WORLDNODE, '');
            WriteGenericProp(Level + 8, PROP_TYPE, 'brush');
            WriteGenericProp(Level + 8, PROP_BRUSHINDEX, IntToStr(k));
            WriteGenericProp(Level + 8, PROP_NODEID, IntToStr(nNodeCounter));
            WriteGenericProp(Level + 8, PROP_FLAGS, '( )');
            WriteArrayStart(Level + 8, ARRAY_PROPERTIES, '');
            WriteGenericPropStr(Level + 9, PROP_NAME, 'Brush');
            WriteGenericProp(Level + 9, PROP_PROPID, IntToStr(m_pReader.ObjectList.nNumObjects {%H-}+ k + 1));
            WriteArrayEnd(Level + 8);
            WriteArrayEnd(Level + 7);
            WriteNodeEnd(Level + 6);
          end;
          WriteArrayEnd(Level + 5);
        end;
      end;
    end;

    WriteNodeEnd(Level + 4);
    WriteArrayEnd(Level + 3);

    Inc(nNodeCounter, 1);
  end;

  WriteNodeEnd(Level + 2);


  WriteArrayEnd(Level + 1);

  WriteArrayEnd(Level);
end;

procedure TLTAWorldExporter.WritePropList(Level: Integer; nIndex: Cardinal);
var i: Cardinal;
    pWorldObject: TLTWorldObject;
begin
  pWorldObject := TLTWorldObject(m_pReader.ObjectList.pObjectList.Items[nIndex]);
  WriteNodeStart(Level, NODE_PROPLIST, '');

  // debug START
  if g_bDebugProps then
    WriteLineComment(Level + 1, 'prop #' + {%H-}IntToStr(nIndex + 1));
  // debug END

  //WriteGenericProp(Level + 1, '  propid ', IntToStr(nCounter));
  if pWorldObject.NumProperties > 0 then
  for i := 0 to pWorldObject.NumProperties - 1 do
  begin
    WritePropData(Level, TLTWorldObjectProperty(pWorldObject.PropertyList.Items[i]));
  end;
  WriteNodeEnd(Level);
end;

procedure TLTAWorldExporter.WriteBrushPropList(Level: Integer; nIndex: Cardinal);
var i: Cardinal;
    pBrush: TLTWorldBrush;
begin
  pBrush := TLTWorldBrush(m_pBrushPropList.Items[nIndex]);
  WriteNodeStart(Level, NODE_PROPLIST, '');

  // debug START
  if g_bDebugProps then
    WriteLineComment(Level + 1, 'prop #' + {%H-}IntToStr(m_pReader.ObjectList.nNumObjects + nIndex + 1) + ' brush #' + IntToStr(nIndex));
  // debug END

  //WriteGenericProp(Level + 1, '  propid ', IntToStr(nCounter));
  if pBrush.NumProperties > 0 then
  for i := 0 to pBrush.NumProperties - 1 do
  begin
    WritePropData(Level, TLTWorldObjectProperty(pBrush.PropertyList.Items[i]));
  end;
  WriteNodeEnd(Level);
end;

procedure TLTAWorldExporter.WritePropData(Level: Integer; pObject: TLTWorldObjectProperty);
var szExtData: string;
begin
  szExtData := GetExtData(pObject);
  case pObject.PropCode of
    PT_STRING:
    begin
      WriteGenericPropExt(Level + 1, PROP_STRING, pObject.PropName , szExtData, '"' + pObject.DataStr + '"');
    end;
    PT_VECTOR:
    begin
      WriteGenericPropExt(Level + 1, PROP_VECTOR, pObject.PropName , szExtData, '( vector (' + LTVectorToStrC(@pObject.DataVec) + ') )');
    end;
    PT_COLOR:
    begin
      WriteGenericPropExt(Level + 1, PROP_COLOR, pObject.PropName , szExtData, '( vector (' + LTVectorToStrC(@pObject.DataVec) + ') )');
    end;
    PT_REAL:
    begin
      WriteGenericPropExt(Level + 1, PROP_REAL, pObject.PropName , szExtData, FormatFloat('0.000000', pObject.DataReal));
    end;
    PT_LONGINT:
    begin
      WriteGenericPropExt(Level + 1, PROP_LONGINT, pObject.PropName , szExtData, FormatFloat('0.000000', PLTFloat(@pObject.DataInt)^));
    end;
    PT_FLAGS:
    begin
      WriteGenericPropExt(Level + 1, PROP_FLAGS, pObject.PropName , szExtData, FormatFloat('0.000000', PLTFloat(@pObject.DataInt)^));
    end;
    PT_BOOL:
    begin
      WriteGenericPropExt(Level + 1, PROP_BOOL, pObject.PropName , szExtData, IntToStr(pObject.DataBool));
    end;
    PT_ROTATION:
    begin
      WriteGenericPropExt(Level + 1, PROP_ROTATION, pObject.PropName , szExtData, '(eulerangles (' + LTRotationIgnoreWToStrC(@pObject.DataRot) + ') )');
    end;
  end;
end;

procedure TLTAWorldExporter.WriteGlobalPropList(Level: Integer);
var i: Cardinal;
    nCounter: Cardinal;
    //pObject: TLTWorldObject;
begin
  nCounter := 1;
  WriteNodeStart(Level, NODE_GLOBALPROPLIST, '');

  WriteNodeStart(Level + 1, NODE_PROPLIST, '');
  WriteNodeEnd(Level + 1);

  if m_pReader.ObjectList.nNumObjects > 0 then
  for i := 0 to m_pReader.ObjectList.nNumObjects - 1 do
  begin
    //pObject := TLTWorldObject(m_pReader.ObjectList.pObjectList.Items[i]);

    {if (m_bIgnoreObjects) and
    (Pos('WorldModel', pObject.GetObjectName) = 0) then
    begin
      WriteLn('--- Skipping object: ', pObject.GetObjectName);
      Continue;
    end;}
    WritePropList(Level + 1, i);
    Inc(nCounter, 1);
  end;
  // brush props
  for i := 0 to m_pBrushPropList.Count - 1 do
  begin
    WriteBrushPropList(Level + 1, i);
    Inc(nCounter, 1);
  end;
  WriteNodeEnd(Level);
end;

procedure TLTAWorldExporter.WriteNavigatorPosList(Level: Integer);
begin
  WriteNodeStart(Level, NODE_NAVIGATORPOSLIST, '');
  WriteNodeEnd(Level);
end;

procedure TLTAWorldExporter.WriteLTWorld;
begin
  m_slExport.Add('');
  WriteArrayStart(0, ARRAY_WORLDHEADER, '');
  WriteHeader(1);
  WritePolyhedronList(1);
  WriteNodeHierarchy(1);
  WriteGlobalPropList(1);
  WriteNavigatorPosList(1);
  WriteArrayEnd(0);
end;

end.

