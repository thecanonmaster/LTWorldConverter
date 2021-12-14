unit edworldexporter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globals, ltworldreader, ltworldtypes, ltworlddata,
  ltworldobject, Contnrs;

const
  ED_VERSION = 1247;
  BGT_SIMPLE = 0;
  BGT_POLY = 1;

  NODE_NODE = 0;
  NODE_BRUSH = 1;
  NODE_OBJECT = 2;

  NODE_NAME_ROOT = 'WorldRoot';
  NODE_NAME_GROUP = 'Group';
  NODE_NAME_BRUSH = 'Brush';

  BRUSH_PROP_NAME = 0;
  BRUSH_PROP_NAME_STR = 'Name';
  BRUSH_PROP_POS = 1;
  BRUSH_PROP_POS_STR = 'Pos';
  BRUSH_PROP_ROTATION = 2;
  BRUSH_PROP_ROTATION_STR = 'Rotation';
  BRUSH_PROP_SOLID = 3;
  BRUSH_PROP_SOLID_STR = 'Solid';
  BRUSH_PROP_NONEXISTANT = 4;
  BRUSH_PROP_NONEXISTANT_STR = 'Nonexistant';
  BRUSH_PROP_INVISIBLE = 5;
  BRUSH_PROP_INVISIBLE_STR = 'Invisible';
  BRUSH_PROP_TRANSLUCENT = 6;
  BRUSH_PROP_TRANSLUCENT_STR = 'Translucent';
  BRUSH_PROP_SKYPORTAL = 7;
  BRUSH_PROP_SKYPORTAL_STR = 'SkyPortal';
  BRUSH_PROP_FULLYBRIGHT = 8;
  BRUSH_PROP_FULLYBRIGHT_STR = 'FullyBright';
  BRUSH_PROP_FLATSHADE = 9;
  BRUSH_PROP_FLATSHADE_STR = 'FlatShade';
  BRUSH_PROP_GOURAUDSHADE = 10;
  BRUSH_PROP_GOURAUDSHADE_STR = 'GouraudShade';
  BRUSH_PROP_LIGHTMAP = 11;
  BRUSH_PROP_LIGHTMAP_STR = 'LightMap';
  BRUSH_PROP_SUBDIVIDE = 12;
  BRUSH_PROP_SUBDIVIDE_STR = 'Subdivide';
  BRUSH_PROP_HULLMAKER = 13;
  BRUSH_PROP_HULLMAKER_STR = 'HullMaker';
  BRUSH_PROP_ALWAYSLIGHTMAP = 14;
  BRUSH_PROP_ALWAYSLIGHTMAP_STR = 'AlwaysLightMap';
  BRUSH_PROP_DIRECTIONALLIGHT = 15;
  BRUSH_PROP_DIRECTIONALLIGHT_STR = 'DirectionalLight';
  BRUSH_PROP_PORTAL = 16;
  BRUSH_PROP_PORTAL_STR = 'Portal';
  BRUSH_PROP_NOSNAP = 17;
  BRUSH_PROP_NOSNAP_STR = 'NoSnap';
  BRUSH_PROP_SKYPAN = 18;
  BRUSH_PROP_SKYPAN_STR = 'SkyPan';
  BRUSH_PROP_DETAILLEVEL = 19;
  BRUSH_PROP_DETAILLEVEL_STR = 'DetailLevel';
  BRUSH_PROP_EFFECT = 20;
  BRUSH_PROP_EFFECT_STR = 'Effect';
  BRUSH_PROP_EFFECTPARAM = 21;
  BRUSH_PROP_EFFECTPARAM_STR = 'EffectParam';
  BRUSH_PROP_FRICTIONCOEFFICIENT = 22;
  BRUSH_PROP_FRICTIONCOEFFICIENT_STR = 'FrictionCoefficient';
  BRUSH_PROP_MAX = BRUSH_PROP_FRICTIONCOEFFICIENT + 1;

type

  { TEDWorldBpsWrapper }

  TEDWorldBspWrapper = class(TObject)
    bAttachStatus: Boolean;
    pWorldBsp: TLTWorldBsp;
    adwBrushIndices: array[0..1] of Cardinal;
  end;

  { TEDWorldExporter }

  TEDWorldExporter = class(TObject)
  private
    m_pExportStream: TMemoryStream;
    m_pReader: TLTWorldReader;
    m_nBrushGenType: Integer;
    m_slModelList: TStringList;
    m_slClassList: TStringList;
    m_slClassesWithBrushes: TStringList;
    m_slObjectsWithBrushes : TStringList;

    m_apBrushProperties: array[0..BRUSH_PROP_MAX - 1] of TLTWorldObjectProperty;
    m_nNodeID: Cardinal;

    procedure WriteAll;
    procedure ReportUnattachedBrushes;
    procedure WriteHeader;
    procedure WriteBrushes;
    procedure BuildHelperLists(nLength: Cardinal; pObjectList: TFPObjectList);
    function WriteSimpleBrush(pModel: TLTWorldBsp): Cardinal;
    function WritePolyBrushes(pModel: TLTWorldBsp): Cardinal;
    procedure WriteRootNode;
    procedure WriteBrushGroupNode;
    procedure WriteObjectGroupNode(strClassName: string);
    procedure WriteBrushNode(strBspName: string; dwIndex: Cardinal; pSurface: TLTWorldSurface);
    procedure FillBrushProperties(strName: string; pSurface: TLTWorldSurface);
    procedure WriteObjectNode(strClassName: string; pObject: TLTWorldObject);
    procedure WriteProperty(pProperty: TLTWorldObjectProperty);
  public
    procedure ExportFile(S: string);
    constructor Create(pReader: TLTWorldReader; slClassesWithBrushes: TStringList);
    destructor Destroy; override;
  end;

var
  g_anColor: array[0..2] of Byte = (255, 255, 255);
  g_anShade: array[0..2] of Byte = (0, 0, 0);

implementation

{ TEDWorldExporter }

procedure TEDWorldExporter.WriteAll;
begin
  WriteHeader;
  WriteBrushes;
  WriteRootNode;
  ReportUnattachedBrushes;
end;

procedure TEDWorldExporter.ReportUnattachedBrushes;
var i: Integer;
    j: Cardinal;
    pObject: TLTWorldObject;
    pWrapper: TEDWorldBspWrapper;
    strBuffer: string;
begin
  for i := 0 to m_slModelList.Count - 1 do
  begin
    pWrapper := TEDWorldBspWrapper(m_slModelList.Objects[i]);
    if not pWrapper.bAttachStatus then
    begin
      strBuffer := '';
      for j := 0 to m_pReader.ObjectList.nNumObjects - 1 do
      begin
        pObject := TLTWorldObject(m_pReader.ObjectList.pObjectList.Items[j]);
        if pObject.GetObjectName = pWrapper.pWorldBsp.WorldName then
          strBuffer := strBuffer + ' ' + pObject.TypeString;
      end;
      WriteLn('WARNING! ', 'Unattached brush - ', pWrapper.pWorldBsp.WorldName, ', matched classes:', strBuffer);
    end;
  end;
end;

procedure TEDWorldExporter.WriteHeader;
var strTemp: String;
    adwDummies: array[0..7] of Cardinal = (0, 0, 0, 0, 0, 0, 0, 0);
begin
  m_pExportStream.WriteDWord(ED_VERSION);
  m_pExportStream.WriteByte(0);

  if g_szBrushGenType = 'simple' then m_nBrushGenType := BGT_SIMPLE
  else if g_szBrushGenType = 'poly' then m_nBrushGenType := BGT_POLY;

  strTemp := m_pReader.Properties;
  m_pExportStream.WriteDWord(strTemp.Length);
  m_pExportStream.WriteBuffer(strTemp[1], strTemp.Length);
  m_pExportStream.WriteBuffer(adwDummies, SizeOf(Cardinal) * 8);
end;

procedure TEDWorldExporter.WriteBrushes;
var i: Cardinal;
    ddwBrushCountPos, ddwEndPos: Int64;
    dwBrushCount: Cardinal = 0;
    pModel: TLTWorldBsp;
    pWrapper: TEDWorldBspWrapper;
begin
  ddwBrushCountPos := m_pExportStream.Position;
  m_pExportStream.WriteDWord(0);

  for i := 0 to m_pReader.ModelList.nNumModels - 1 do
  begin
    pModel := TLTWorldData(m_pReader.ModelList.pModelList.Items[i]).OriginalBSP;
    pWrapper := TEDWorldBspWrapper.Create;
    pWrapper.adwBrushIndices[0] := dwBrushCount;
    pWrapper.pWorldBsp := pModel;

    m_slModelList.AddObject(pModel.WorldName, pWrapper);

    if pModel.WorldName <> BSP_PHYSICS then
    begin
      pWrapper.bAttachStatus := False;
      Inc(dwBrushCount, WriteSimpleBrush(pModel));
    end
    else
    begin
      pWrapper.bAttachStatus := True;
      if m_nBrushGenType = BGT_POLY then
        Inc(dwBrushCount, WritePolyBrushes(pModel))
      else if m_nBrushGenType = BGT_SIMPLE then
        Inc(dwBrushCount, WriteSimpleBrush(pModel));
    end;

    pWrapper.adwBrushIndices[1] := dwBrushCount;
  end;

  ddwEndPos := m_pExportStream.Position;
  m_pExportStream.Seek(ddwBrushCountPos, soFromBeginning);
  m_pExportStream.WriteDWord(dwBrushCount);
  m_pExportStream.Seek(ddwEndPos, soFromBeginning);

  m_slModelList.SaveToFile(g_szDumpsDir + g_szPathSep + 'ModelList.txt');
end;

function TEDWorldExporter.WriteSimpleBrush(pModel: TLTWorldBsp): Cardinal;
var i, j: Cardinal;
    nLen: Word;
    pVertex: TLTWorldVertex;
    pPlane: TLTWorldPlane;
    pSurface: TLTWorldSurface;
    pPoly: TLTWorldPoly;

    // TEMP
    aFloat5: array[0..4] of LTFloat = (1, 1, 0, 0, 0);
begin
  Result := 1;
  m_pExportStream.WriteBuffer(g_anColor, 3);
  m_pExportStream.WriteDWord(pModel.Points);

  if pModel.Points > 0 then
  for i := 0 to pModel.Points - 1 do
  begin
    pVertex := TLTWorldVertex(pModel.PointsList.Items[i]);
    m_pExportStream.WriteBuffer(pVertex.m_vData, SizeOf(LTVector));
  end;

  m_pExportStream.WriteDWord(pModel.Polies);

  if pModel.Polies > 0 then
  for i := 0 to pModel.Polies - 1 do
  begin
    pPoly := TLTWorldPoly(pModel.PoliesList.Items[i]);

    m_pExportStream.WriteDWord(pPoly.LoVerts);
    if pPoly.LoVerts > 0 then
    for j := 0 to pPoly.LoVerts - 1 do
    begin
      m_pExportStream.WriteWord(pPoly.DiskVerts[j].nVerts);
    end;

    pSurface := TLTWorldSurface(pModel.SurfacesList.Items[pPoly.Surface]);
    pPlane := TLTWorldPlane(pModel.PlanesList.Items[pSurface.m_nPlane]);

    m_pExportStream.WriteBuffer(pPlane.m_vNormal, SizeOf(LTVector));
    m_pExportStream.WriteBuffer(pPlane.m_fDist, SizeOf(LTFloat));

    m_pExportStream.WriteBuffer(aFloat5, SizeOf(LTFloat) * 5);
    // TEMP
    m_pExportStream.WriteDWord(0);


    nLen := pModel.TextureNames[pSurface.m_nTexture].Length;
    m_pExportStream.WriteWord(nLen);
    m_pExportStream.WriteBuffer(pModel.TextureNames[pSurface.m_nTexture][1], nLen);
    m_pExportStream.WriteDWord(0);
    m_pExportStream.WriteBuffer(g_anShade, 3);
  end;
end;

function TEDWorldExporter.WritePolyBrushes(pModel: TLTWorldBsp): Cardinal;
var i, j: Cardinal;
    nLen: Word;
    pPoly: TLTWorldPoly;
    pVertex: TLTWorldVertex;
    pPlane: TLTWorldPlane;
    pSurface: TLTWorldSurface;

    // TEMP
    aFloat5: array[0..4] of LTFloat = (1, 1, 0, 0, 0);
begin
  Result := pModel.Polies;
  for i := 0 to pModel.Polies - 1 do
  begin
    pPoly := TLTWorldPoly(pModel.PoliesList.Items[i]);
    m_pExportStream.WriteBuffer(g_anColor, 3);
    m_pExportStream.WriteDWord(pPoly.LoVerts);

    if pPoly.LoVerts > 0 then
    begin
      for j := 0 to pPoly.LoVerts - 1 do
      begin
        pVertex := TLTWorldVertex(pModel.PointsList.Items[pPoly.DiskVerts[j].nVerts]);
        m_pExportStream.WriteBuffer(pVertex.m_vData, SizeOf(LTVector));
      end;
    end;

    m_pExportStream.WriteDWord(1);
    m_pExportStream.WriteDWord(pPoly.LoVerts);

    for j := 0 to pPoly.LoVerts - 1 do
      m_pExportStream.WriteWord(j);

    pSurface := TLTWorldSurface(pModel.SurfacesList.Items[pPoly.Surface]);
    pPlane := TLTWorldPlane(pModel.PlanesList.Items[pSurface.m_nPlane]);

    m_pExportStream.WriteBuffer(pPlane.m_vNormal, SizeOf(LTVector));
    m_pExportStream.WriteBuffer(pPlane.m_fDist, SizeOf(LTFloat));

    m_pExportStream.WriteBuffer(aFloat5, SizeOf(LTFloat) * 5);
    // TEMP
    m_pExportStream.WriteDWord(0);

    nLen := pModel.TextureNames[pSurface.m_nTexture].Length;
    m_pExportStream.WriteWord(nLen);
    m_pExportStream.WriteBuffer(pModel.TextureNames[pSurface.m_nTexture][1], nLen);
    m_pExportStream.WriteDWord(0);
    m_pExportStream.WriteBuffer(g_anShade, 3);
  end;
end;

procedure TEDWorldExporter.BuildHelperLists(nLength: Cardinal; pObjectList: TFPObjectList);
var i: Cardinal;
    pObject: TLTWorldObject;
begin
  m_slClassList.Add(BSP_PHYSICS);
  if nLength > 0 then
  for i := 0 to nLength - 1 do
  begin
    pObject := TLTWorldObject(pObjectList.Items[i]);
    if m_slClassList.IndexOf(pObject.TypeString) = -1 then
      m_slClassList.Add(pObject.TypeString);
  end;
  //m_slClassList.Add(BSP_PHYSICS);
  m_slClassList.SaveToFile(g_szDumpsDir + g_szPathSep + 'ClassList.txt');
end;

procedure TEDWorldExporter.WriteRootNode;
var i: Integer;
begin
  // Children count + node
  m_pExportStream.WriteWord(m_slClassList.Count);

  m_nNodeID := 2;
  for i := 0 to m_slClassList.Count - 1 do
  begin
    if m_slClassList.Strings[i] = BSP_PHYSICS then
      WriteBrushGroupNode
    else
      WriteObjectGroupNode(m_slClassList.Strings[i]);
  end;

  // Data length + zero object name + zero prop count
  m_pExportStream.WriteWord(6);
  m_pExportStream.WriteWord(0);
  m_pExportStream.WriteDWord(0);

  // IDs
  m_pExportStream.WriteDWord(1);
  m_pExportStream.WriteDWord(0);

  // Display name
  m_pExportStream.WriteWord(NODE_NAME_ROOT.Length);
  m_pExportStream.WriteBuffer(NODE_NAME_ROOT[1], NODE_NAME_ROOT.Length);
end;

procedure TEDWorldExporter.WriteBrushGroupNode;
var nIndex: Integer;
    i: Cardinal;
    pWrapper: TEDWorldBspWrapper;
    pSurface: TLTWorldSurface;
    strName: string;
    pPoly: TLTWorldPoly;
begin
  nIndex := m_slModelList.IndexOf(BSP_PHYSICS);
  pWrapper := TEDWorldBspWrapper(m_slModelList.Objects[nIndex]);

  // Node + children count
  m_pExportStream.WriteDWord(NODE_NODE);
  m_pExportStream.WriteWord(pWrapper.adwBrushIndices[1] - pWrapper.adwBrushIndices[0]);

  if m_nBrushGenType = BGT_POLY then
  begin
    for i := 0 to pWrapper.pWorldBsp.Polies - 1 do
    begin
      pPoly := TLTWorldPoly(pWrapper.pWorldBsp.PoliesList.Items[i]);
      pSurface := TLTWorldSurface(pWrapper.pWorldBsp.SurfacesList.Items[pPoly.Surface]);
      WriteBrushNode(BSP_PHYSICS, pWrapper.adwBrushIndices[0] + i, pSurface);
    end;
  end
  else if m_nBrushGenType = BGT_SIMPLE then
  begin
    pSurface := TLTWorldSurface(pWrapper.pWorldBsp.SurfacesList.Items[0]);
    WriteBrushNode(BSP_PHYSICS, pWrapper.adwBrushIndices[0], pSurface);
  end;

  // Data length + zero object name + zero prop count
  m_pExportStream.WriteWord(6);
  m_pExportStream.WriteWord(0);
  m_pExportStream.WriteDWord(0);

  // IDs
  m_pExportStream.WriteDWord(m_nNodeID);
  m_pExportStream.WriteDWord(0);
  Inc(m_nNodeID, 1);

  // Display name
  strName := BSP_PHYSICS + NODE_NAME_GROUP;
  m_pExportStream.WriteWord(strName.Length);
  m_pExportStream.WriteBuffer(strName[1], strName.Length);
end;

procedure TEDWorldExporter.WriteObjectGroupNode(strClassName: string);
var i, nCount: Cardinal;
    pObject: TLTWorldObject;
    strNodeName: string;
    ddwStart, ddwEnd: Int64;
begin
  // Node + children count
  m_pExportStream.WriteDWord(NODE_NODE);
  ddwStart := m_pExportStream.Position;
  m_pExportStream.WriteWord(0);

  nCount := 0;
  for i := 0 to m_pReader.ObjectList.nNumObjects - 1 do
  begin
    pObject := TLTWorldObject(m_pReader.ObjectList.pObjectList.Items[i]);
    if pObject.TypeString = strClassName then
    begin
      WriteObjectNode(strClassName, pObject);
      Inc(nCount, 1);
    end;
  end;

  // Data length + zero object name + zero prop count
  m_pExportStream.WriteWord(6);
  m_pExportStream.WriteWord(0);
  m_pExportStream.WriteDWord(0);

  // IDs
  m_pExportStream.WriteDWord(m_nNodeID);
  m_pExportStream.WriteDWord(0);
  Inc(m_nNodeID, 1);

  // Display name
  strNodeName := strClassName + NODE_NAME_GROUP;
  m_pExportStream.WriteWord(strNodeName.Length);
  m_pExportStream.WriteBuffer(strNodeName[1], strNodeName.Length);

  ddwEnd := m_pExportStream.Position;
  m_pExportStream.Seek(ddwStart, soBeginning);
  m_pExportStream.WriteWord(nCount);
  m_pExportStream.Seek(ddwEnd, soBeginning);
end;

procedure TEDWorldExporter.WriteBrushNode(strBspName: string; dwIndex: Cardinal; pSurface: TLTWorldSurface);
var ddwStart, ddwEnd: Int64;
    i: Integer;
    strName: string;
begin
  // Brush + brush index + no children
  m_pExportStream.WriteDWord(NODE_BRUSH);
  m_pExportStream.WriteDWord(dwIndex);
  m_pExportStream.WriteWord(0);

  // Data length
  ddwStart := m_pExportStream.Position;
  m_pExportStream.WriteWord(0);

  // Object name
  strName := NODE_NAME_BRUSH;
  m_pExportStream.WriteWord(strName.Length);
  m_pExportStream.WriteBuffer(strName[1], strName.Length);

  // Prop count + props
  m_pExportStream.WriteDWord(BRUSH_PROP_MAX);
  strName := strName + '_' + strBspName + '_' + dwIndex.ToString;

  FillBrushProperties(strName, pSurface);
  for i := 0 to BRUSH_PROP_MAX - 1 do
    WriteProperty(m_apBrushProperties[i]);

  ddwEnd := m_pExportStream.Position;
  m_pExportStream.Seek(ddwStart, soBeginning);
  m_pExportStream.WriteWord(ddwEnd - ddwStart - 2);
  m_pExportStream.Seek(ddwEnd, soBeginning);

  // IDs
  m_pExportStream.WriteDWord(m_nNodeID);
  m_pExportStream.WriteDWord(0);

  Inc(m_nNodeID, 1);

  // Display name
  m_pExportStream.WriteWord(strName.Length);
  m_pExportStream.WriteBuffer(strName[1], strName.Length);
end;

procedure TEDWorldExporter.FillBrushProperties(strName: string; pSurface: TLTWorldSurface);
begin
  m_apBrushProperties[BRUSH_PROP_NAME].DataStr := strName;
  m_apBrushProperties[BRUSH_PROP_POS].DataVec := LTVectorInit(0, 0, 0);
  m_apBrushProperties[BRUSH_PROP_ROTATION].DataRot := LTRotationInit(0, 0, 0, 1);
  m_apBrushProperties[BRUSH_PROP_SOLID].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_SOLID);
  m_apBrushProperties[BRUSH_PROP_NONEXISTANT].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_NONEXISTANT);
  m_apBrushProperties[BRUSH_PROP_INVISIBLE].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_INVISIBLE);
  m_apBrushProperties[BRUSH_PROP_TRANSLUCENT].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_TRANSPARENT);
  m_apBrushProperties[BRUSH_PROP_SKYPORTAL].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_SKY);
  m_apBrushProperties[BRUSH_PROP_FULLYBRIGHT].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_BRIGHT);
  m_apBrushProperties[BRUSH_PROP_FLATSHADE].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_FLATSHADE);
  m_apBrushProperties[BRUSH_PROP_GOURAUDSHADE].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_GOURAUDSHADE);
  m_apBrushProperties[BRUSH_PROP_LIGHTMAP].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_LIGHTMAP);
  m_apBrushProperties[BRUSH_PROP_SUBDIVIDE].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_NOSUBDIV, True);
  m_apBrushProperties[BRUSH_PROP_HULLMAKER].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_HULLMAKER);
  m_apBrushProperties[BRUSH_PROP_ALWAYSLIGHTMAP].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_ALWAYSLIGHTMAP);
  m_apBrushProperties[BRUSH_PROP_DIRECTIONALLIGHT].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_DIRECTIONALLIGHT);
  m_apBrushProperties[BRUSH_PROP_PORTAL].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_PORTAL);
  m_apBrushProperties[BRUSH_PROP_NOSNAP].DataBool := 1;
  m_apBrushProperties[BRUSH_PROP_SKYPAN].DataBool := FlagsToBool(pSurface.m_nFlags, SURF_PANNINGSKY);
  m_apBrushProperties[BRUSH_PROP_DETAILLEVEL].DataInt := FlagsToBool(pSurface.m_nFlags, SURF_INVISIBLE); // Why not
  m_apBrushProperties[BRUSH_PROP_EFFECT].DataStr := pSurface.m_szEffect;
  m_apBrushProperties[BRUSH_PROP_EFFECTPARAM].DataStr := pSurface.m_szEffectParam;
  m_apBrushProperties[BRUSH_PROP_FRICTIONCOEFFICIENT].DataReal := 1;
end;

procedure TEDWorldExporter.WriteObjectNode(strClassName: string; pObject: TLTWorldObject);
var strObjectName: string;
    i: Cardinal;
    nIndex, nDupIndex: Integer;
    pProperty: TLTWorldObjectProperty;
    ddwStart, ddwEnd: Int64;
    pWrapper: TEDWorldBspWrapper;
    pSurface: TLTWorldSurface;

begin
  strObjectName := pObject.GetObjectName;
  nDupIndex := m_slObjectsWithBrushes.IndexOf(strObjectName);

  if nDupIndex = -1 then
    nIndex := m_slModelList.IndexOf(strObjectName)
  else
    nIndex := (m_slModelList as TStrings).IndexOf(strObjectName, Integer(m_slObjectsWithBrushes.Objects[nDupIndex]) + 1);

  // Object + no children
  m_pExportStream.WriteDWord(NODE_OBJECT);
  if (nIndex > -1) and (m_slClassesWithBrushes.IndexOf(pObject.TypeString) > -1) then
  begin
    if nDupIndex = -1 then
      m_slObjectsWithBrushes.AddObject(strObjectName, TObject(nIndex))
    else
      m_slObjectsWithBrushes.Objects[nDupIndex] := TObject(nIndex);

    m_pExportStream.WriteWord(1);
    pWrapper := TEDWorldBspWrapper(m_slModelList.Objects[nIndex]);
    pWrapper.bAttachStatus := True;
    pSurface := TLTWorldSurface(pWrapper.pWorldBsp.SurfacesList.Items[0]);
    WriteBrushNode(m_slModelList.Strings[nIndex], pWrapper.adwBrushIndices[0], pSurface);
  end
  else
  begin
    m_pExportStream.WriteWord(0);
  end;

  // Data length
  ddwStart := m_pExportStream.Position;
  m_pExportStream.WriteWord(0);

  // Object name
  m_pExportStream.WriteWord(strClassName.Length);
  m_pExportStream.WriteBuffer(strClassName[1], strClassName.Length);

  // Prop count + props
  m_pExportStream.WriteDWord(pObject.NumProperties);
  if pObject.NumProperties > 0 then
  for i := 0 to pObject.NumProperties - 1 do
  begin
    pProperty := TLTWorldObjectProperty(pObject.PropertyList.Items[i]);
    WriteProperty(pProperty);
  end;

  ddwEnd := m_pExportStream.Position;
  m_pExportStream.Seek(ddwStart, soBeginning);
  m_pExportStream.WriteWord(ddwEnd - ddwStart - 2);
  m_pExportStream.Seek(ddwEnd, soBeginning);

  // IDs
  m_pExportStream.WriteDWord(m_nNodeID);
  m_pExportStream.WriteDWord(0);
  Inc(m_nNodeID, 1);

  // Display name
  m_pExportStream.WriteWord(strObjectName.Length);
  m_pExportStream.WriteBuffer(strObjectName[1], strObjectName.Length);
end;

procedure TEDWorldExporter.WriteProperty(pProperty: TLTWorldObjectProperty);
var strTemp: string;
    rTemp: LTRotation;
    fTemp: LTFloat;
    vTemp: LTVector;
begin
  strTemp := pProperty.PropName;
  m_pExportStream.WriteWord(strTemp.Length);
  m_pExportStream.WriteBuffer(strTemp[1], strTemp.Length);
  m_pExportStream.WriteByte(pProperty.PropCode);
  m_pExportStream.WriteDWord(pProperty.PropFlags);
  //m_pExportStream.WriteWord(pProperty.PropDataLength);

  case pProperty.PropCode of
    PT_STRING:
      begin
        strTemp := pProperty.DataStr;
        m_pExportStream.WriteWord(SizeOf(Word) + strTemp.Length);
        m_pExportStream.WriteWord(strTemp.Length);
        if strTemp.Length > 0 then
          m_pExportStream.WriteBuffer(strTemp[1], strTemp.Length);
      end;
    PT_VECTOR, PT_COLOR:
      begin
        vTemp := pProperty.DataVec;
        m_pExportStream.WriteWord(SizeOf(LTVector));
        m_pExportStream.WriteBuffer(vTemp, SizeOf(LTVector));
      end;
    PT_REAL:
      begin
        fTemp := pProperty.DataReal;
        m_pExportStream.WriteWord(SizeOf(LTFloat));
        m_pExportStream.WriteBuffer(fTemp, SizeOf(LTFloat));
      end;
    PT_BOOL:
      begin
        m_pExportStream.WriteWord(SizeOf(Byte));
        m_pExportStream.WriteByte(pProperty.DataBool);
      end;
    PT_FLAGS, PT_LONGINT:
      begin
        m_pExportStream.WriteWord(SizeOf(Cardinal));
        m_pExportStream.WriteDWord(pProperty.DataInt);
      end;
    PT_ROTATION:
      begin
        rTemp := pProperty.DataRot;
        m_pExportStream.WriteWord(SizeOf(LTRotation));
        m_pExportStream.WriteBuffer(rTemp, sizeof(LTRotation));
      end;
  end;
end;

procedure TEDWorldExporter.ExportFile(S: string);
begin
  WriteAll;
  m_pExportStream.SaveToFile(S);
end;

constructor TEDWorldExporter.Create(pReader: TLTWorldReader; slClassesWithBrushes: TStringList);
begin
  m_pExportStream := TMemoryStream.Create;

  m_slModelList := TStringList.Create;
  m_slModelList.CaseSensitive := True;

  m_slClassList := TStringList.Create;
  m_slClassList.CaseSensitive := True;
  m_pReader := pReader;

  m_slClassesWithBrushes := slClassesWithBrushes;
  m_slClassesWithBrushes.CaseSensitive := True;

  m_slObjectsWithBrushes := TStringList.Create;
  m_slObjectsWithBrushes.CaseSensitive := True;

  //m_pReader.MoveWorldModel(BSP_PHYSICS, m_pReader.ModelList.pModelList.Count - 1);

  m_apBrushProperties[BRUSH_PROP_NAME] := TLTWorldObjectProperty.CreateStr(BRUSH_PROP_NAME_STR, 0);
  m_apBrushProperties[BRUSH_PROP_POS] := TLTWorldObjectProperty.CreateVec(BRUSH_PROP_POS_STR, 0);
  m_apBrushProperties[BRUSH_PROP_ROTATION] := TLTWorldObjectProperty.CreateRot(BRUSH_PROP_ROTATION_STR, 0);
  m_apBrushProperties[BRUSH_PROP_SOLID] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_SOLID_STR, 0);
  m_apBrushProperties[BRUSH_PROP_NONEXISTANT] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_NONEXISTANT_STR, 0);
  m_apBrushProperties[BRUSH_PROP_INVISIBLE] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_INVISIBLE_STR, 0);
  m_apBrushProperties[BRUSH_PROP_TRANSLUCENT] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_TRANSLUCENT_STR, 0);
  m_apBrushProperties[BRUSH_PROP_SKYPORTAL] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_SKYPORTAL_STR, 0);
  m_apBrushProperties[BRUSH_PROP_FULLYBRIGHT] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_FULLYBRIGHT_STR, 0);
  m_apBrushProperties[BRUSH_PROP_FLATSHADE] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_FLATSHADE_STR, 0);
  m_apBrushProperties[BRUSH_PROP_GOURAUDSHADE] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_GOURAUDSHADE_STR, 0);
  m_apBrushProperties[BRUSH_PROP_LIGHTMAP] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_LIGHTMAP_STR, 0);
  m_apBrushProperties[BRUSH_PROP_SUBDIVIDE] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_SUBDIVIDE_STR, 0);
  m_apBrushProperties[BRUSH_PROP_HULLMAKER] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_HULLMAKER_STR, 0);
  m_apBrushProperties[BRUSH_PROP_ALWAYSLIGHTMAP] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_ALWAYSLIGHTMAP_STR, 0);
  m_apBrushProperties[BRUSH_PROP_DIRECTIONALLIGHT] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_DIRECTIONALLIGHT_STR, 0);
  m_apBrushProperties[BRUSH_PROP_PORTAL] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_PORTAL_STR, 0);
  m_apBrushProperties[BRUSH_PROP_NOSNAP] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_NOSNAP_STR, 0);
  m_apBrushProperties[BRUSH_PROP_SKYPAN] := TLTWorldObjectProperty.CreateBool(BRUSH_PROP_SKYPAN_STR, 0);
  m_apBrushProperties[BRUSH_PROP_DETAILLEVEL] := TLTWorldObjectProperty.CreateInt(BRUSH_PROP_DETAILLEVEL_STR, 0);
  m_apBrushProperties[BRUSH_PROP_EFFECT] := TLTWorldObjectProperty.CreateStr(BRUSH_PROP_EFFECT_STR, 0);
  m_apBrushProperties[BRUSH_PROP_EFFECTPARAM] := TLTWorldObjectProperty.CreateStr(BRUSH_PROP_EFFECTPARAM_STR, 0);
  m_apBrushProperties[BRUSH_PROP_FRICTIONCOEFFICIENT] := TLTWorldObjectProperty.CreateReal(BRUSH_PROP_FRICTIONCOEFFICIENT_STR, 0);

  BuildHelperLists(m_pReader.ObjectList.nNumObjects, m_pReader.ObjectList.pObjectList);
end;

destructor TEDWorldExporter.Destroy;
var i: Integer;
begin
  inherited Destroy;
  m_pExportStream.Free;

  for i := 0 to m_slModelList.Count - 1 do
    m_slModelList.Objects[i].Free;
  m_slModelList.Free;

  m_slClassList.Free;
  m_slObjectsWithBrushes.Free;
end;

end.

