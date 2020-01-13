unit ltworlddata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ltworldtypes, contnrs, globals, MyLogger;

type
  TStringArray = array of string;

  TUnknownStruct = packed record
    n1: Cardinal;
    n2: Cardinal;
    n3: Cardinal;
    v1: LTVector;
    v2: LTVector;
  end;

  TLTWorldSurface = class(TObject)
  public
    m_fUV1: LTVector;
    m_fUV2: LTVector;
    m_fUV3: LTVector;
    m_nTexture: Word;
    m_nFlags: Cardinal;
    m_nUnknown1: Cardinal;
    m_nUseEffect: Byte;
    m_szEffect: string;
    m_szEffectParam: string;
    m_nTextureFlags: Word;
  end;

  TLTWorldPlane = class(TObject)
  public
    m_vNormal: LTVector;
    m_fDist: LTFloat;
  end;

  TLTWorldVertex = class(TObject)
  public
    m_vData: LTVector;
  end;

  TLTDiskVert = packed record
    nVerts: Word;
    nDummy1: Byte;
    nDummy2: Byte;
    nDummy3: Byte;
  end;
  TLTDiskVertList = array[0..MAX_WORLDPOLY_VERTS - 1] of TLTDiskVert;
  TDynCardinalArray = array of Cardinal;
  TLTNodeIndices = array[0..1] of Cardinal;

  TLTWorldNode = class(TObject)
  private
    m_nFlags: Byte;
    m_nPlaneType: Byte;
    m_nPoly: Cardinal;
    m_nLeaf: Word;
  public
    m_anSides: TLTNodeIndices;
    m_anSidesStatus: TLTNodeIndices;

    property Flags: Byte read m_nFlags write m_nFlags;
    property PlaneType: Byte read m_nPlaneType write m_nPlaneType;
    property Leaf: Word read m_nLeaf write m_nLeaf;
    property Poly: Cardinal read m_nPoly write m_nPoly;
    property Sides: TLTNodeIndices read m_anSides write m_anSides;
    property SidesStatus: TLTNodeIndices read m_anSidesStatus write m_anSidesStatus;
  end;

  TLTWorldPoly = class(TObject)
  private
    m_nIndexAndNumVerts: Cardinal;

    m_nLoVerts: Byte;
    m_nHiVerts: Byte;

    m_vCenter: LTVector;
    m_fRadius: LTFloat;

    m_nUnknownFlags: Cardinal;
    m_nUnknownNum: Word;
    m_anUnknownList: TDynCardinalArray;

    m_fUV1: LTVector;
    m_fUV2: LTVector;
    m_fUV3: LTVector;

    m_nPlane: Cardinal;
    m_nSurface: Cardinal;

    m_aDiskVerts: TLTDiskVertList;
  public
    property Radius: LTFloat read m_fRadius write m_fRadius;
    property Center: LTVector read m_vCenter write m_vCenter;
    property UnknownFlags: Cardinal read m_nUnknownFlags write m_nUnknownFlags;
    property UnknownNum: Word read m_nUnknownNum write m_nUnknownNum;
    property UnknownList: TDynCardinalArray read m_anUnknownList write m_anUnknownList;

    property LoVerts: Byte read m_nLoVerts write m_nLoVerts;
    property HiVerts: Byte read m_nHiVerts write m_nHiVerts;

    property UVData1: LTVector read m_fUV1 write m_fUV1;
    property UVData2: LTVector read m_fUV2 write m_fUV2;
    property UVData3: LTVector read m_fUV3 write m_fUV3;
    property DiskVerts: TLTDiskVertList read m_aDiskVerts write m_aDiskVerts;

    property IndexAndNumVerts: Cardinal read m_nIndexAndNumVerts write m_nIndexAndNumVerts;
    property Plane: Cardinal read m_nPlane write m_nPlane;
    property Surface: Cardinal read m_nSurface write m_nSurface;
    function GetNumVertices: Cardinal;
    procedure SetNumVertices(nVertices: Cardinal);
    function GetIndex: Cardinal;
    procedure SetIndex(nIndex: Cardinal);
    procedure ReadPoly(FS: TMemoryStream);
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLTWorldBsp }

  TLTWorldBsp = class(TObject)
  private
    m_nWorldInfoFlags: Word;
    m_szWorldName: string;
    m_nPoints: Cardinal;
    m_nPlanes: Cardinal;
    m_nSurfaces: Cardinal;
    m_nUserPortals: Cardinal;
    m_nPolies: Cardinal;
    m_nLeafs: Cardinal;
    m_nVerts: Cardinal;
    m_nTotalVisListSize: Cardinal;
    m_nLeafLists: Cardinal;
    m_nNodes: Cardinal;
    m_vMinBox: LTVector;
    m_vMaxBox: LTVector;
    m_vWorldTranslation: LTVector;
    m_nNamesLen: Cardinal;
    m_nTextures: Cardinal;
    m_aszTextureNames: TStringArray;
    m_nRootNode: Cardinal;
    m_nRootNodeStatus: Cardinal;

    m_UnknownStruct: TUnknownStruct;

    m_pPolies: TFPObjectList;
    m_pPlanes: TFPObjectList;
    m_pSurfaces: TFPObjectList;
    m_pPoints: TFPObjectList;
    m_pNodes: TFPObjectList;

  protected
  public
    property WorldInfoFlags: Word read m_nWorldInfoFlags write m_nWorldInfoFlags;
    property WorldName: string read m_szWorldName write m_szWorldName;
    property Points: Cardinal read m_nPoints write m_nPoints;
    property Planes: Cardinal read m_nPlanes write m_nPlanes;
    property Surfaces: Cardinal read m_nSurfaces write m_nSurfaces;
    property UserPortals: Cardinal read m_nUserPortals write m_nUserPortals;
    property Polies: Cardinal read m_nPolies write m_nPolies;
    property Leafs: Cardinal read m_nLeafs write m_nLeafs;
    property Verts: Cardinal read m_nVerts write m_nVerts;
    property TotalVisListSize: Cardinal read m_nTotalVisListSize write m_nTotalVisListSize;
    property LeafLists: Cardinal read m_nLeafLists write m_nLeafLists;
    property Nodes: Cardinal read m_nNodes write m_nNodes;
    property Textures: Cardinal read m_nTextures write m_nTextures;

    property RootNode: Cardinal read m_nRootNode write m_nRootNode;
    property RootNodeStatus: Cardinal read m_nRootNodeStatus write m_nRootNodeStatus;

    property MinBox: LTVector read m_vMinBox write m_vMinBox;
    property MaxBox: LTVector read m_vMaxBox write m_vMaxBox;
    property WorldTranslation: LTVector read m_vWorldTranslation write m_vWorldTranslation;

    property TextureNames: TStringArray read m_aszTextureNames write m_aszTextureNames;

    property PoliesList: TFPObjectList read m_pPolies write m_pPolies;
    property PlanesList: TFPObjectList read m_pPlanes write m_pPlanes;
    property SurfacesList: TFPObjectList read m_pSurfaces write m_pSurfaces;
    property PointsList: TFPObjectList read m_pPoints write m_pPoints;
    property NodesList: TFPObjectList read m_pNodes write m_pNodes;

    property UnknownStruct: TUnknownStruct read m_UnknownStruct write m_UnknownStruct;

    function Load(FS: TMemoryStream; bUsePlaneTypes: Boolean): Integer;
    procedure ReadTextures(FS: TMemoryStream);
    procedure ReadPolies(FS: TMemoryStream; bReadData: Boolean);
    procedure ReadLeafs(FS: TMemoryStream);
    procedure ReadPlanes(FS: TMemoryStream);
    procedure ReadSurfaces(FS: TMemoryStream);
    procedure ReadPoints(FS: TMemoryStream);
    procedure ReadNodes(FS: TMemoryStream);

    procedure SaveNodesDump(S: string);

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TLTWorldData = class(TObject)
  private
    m_nFlags: Cardinal;
    m_pOriginalBSP: TLTWorldBsp;
    m_nNextPos: Cardinal;
  protected
  public
    property Flags: Cardinal read m_nFlags write m_nFlags;
    property OriginalBSP: TLTWorldBsp read m_pOriginalBSP write m_pOriginalBSP;
    property NextPos: Cardinal read m_nNextPos write m_nNextPos;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

function w_NodeForIndex(nListSize: Cardinal; nIndex: Integer; var nStatus: Cardinal): Cardinal;
procedure w_SetPlaneTypes(pNodeList: TFPObjectList; pPolyList: TFPObjectList; pPlaneList: TFPObjectList; nNodes: Cardinal; bUsePlaneTypes: Boolean);

implementation

procedure TLTWorldPoly.ReadPoly(FS: TMemoryStream);
//var i: Cardinal;
begin
  FS.Read(m_vCenter, SizeOf(LTVector));
  //FS.Read(m_fRadius, 4);

  FS.Read(m_nUnknownFlags, 4);
  FS.Read(m_nUnknownNum, 2);

  if m_nUnknownNum > 0 then
  begin
    SetLength(m_anUnknownList, m_nUnknownNum);
    FS.Read(m_anUnknownList[0], 4 * m_nUnknownNum);
  end;

  FS.Read(m_nSurface, 4);
  FS.Read(m_nPlane, 4);

  FS.Read(m_fUV1, sizeof(LTVector));
  FS.Read(m_fUV2, sizeof(LTVector));
  FS.Read(m_fUV3, sizeof(LTVector));

  FS.Read(m_aDiskVerts[0], SizeOf(TLTDiskVert) * GetNumVertices);
end;

function TLTWorldPoly.GetIndex: Cardinal;
begin
  Result := (m_nIndexAndNumVerts and $FFFFFF00) shr 8;
end;

procedure TLTWorldPoly.SetIndex(nIndex: Cardinal);
begin
  m_nIndexAndNumVerts := (m_nIndexAndNumVerts and $FF) or (nIndex shl 8);
end;

procedure TLTWorldPoly.SetNumVertices(nVertices: Cardinal);
begin
  m_nIndexAndNumVerts := (m_nIndexAndNumVerts and $FFFFFF00) or (nVertices and $FF)
end;

function TLTWorldPoly.GetNumVertices: Cardinal;
begin
  Result := m_nIndexAndNumVerts and $FF;
end;

constructor TLTWorldData.Create;
begin
  m_pOriginalBSP := nil;
end;

destructor TLTWorldData.Destroy;
begin
  if m_pOriginalBSP <> nil then m_pOriginalBSP.Free;
  inherited;
end;

function TLTWorldBsp.Load(FS: TMemoryStream; bUsePlaneTypes: Boolean): Integer;
var dwWorldInfoFlags, dwUnknown, dwUnknown2, dwUnknown3: Cardinal;
    nNameLen: Word;
    //pVertex: TLTWorldVertex;
begin
  Result := 0;
  dwWorldInfoFlags := 0;
  dwUnknown := 0;
  dwUnknown2 := 0;
  dwUnknown3 := 0;
  nNameLen := 0;
  FS.Read(dwWorldInfoFlags, 4);
  Assert((dwWorldInfoFlags and $ffff0000) = 0, 'ASSERT FAILED!');
  m_nWorldInfoFlags := dwWorldInfoFlags;
  FS.Read(dwUnknown, 4);
  FS.Read(nNameLen, 2);
  SetLength(m_szWorldName, nNameLen);
  FS.Read(m_szWorldName[1], nNameLen);

  WriteLn('--- Loading: ', m_szWorldName);

  FS.Read(m_nPoints, 4);
  FS.Read(m_nPlanes, 4);
  FS.Read(m_nSurfaces, 4);

  FS.Read(m_nUserPortals, 4);
  FS.Read(m_nPolies, 4);
  FS.Read(m_nLeafs, 4);
  FS.Read(m_nVerts, 4);
  FS.Read(m_nTotalVisListSize, 4);
  FS.Read(m_nLeafLists, 4);
  FS.Read(m_nNodes, 4);

  FS.Read(dwUnknown2, 4);
  FS.Read(dwUnknown3, 4);

  FS.Read(m_vMinBox, SizeOf(LTVector));
  FS.Read(m_vMaxBox, SizeOf(LTVector));
  FS.Read(m_vWorldTranslation, SizeOf(LTVector));

  ReadTextures(FS);
  ReadPolies(FS, False);
  // fuck this!
  if m_nLeafs > 0 then
  begin
    Logger.WLog(LM_WARN, 'WorldModel "' + m_szWorldName + '" has leafs > 0');
    Exit(-1);
  end;

  //if m_nLeafs = 0 then
  //   ReadLeafs(FS); //else FS.Position := $1C87;

  ReadPlanes(FS);
  ReadSurfaces(FS);
  ReadPoints(FS);
  ReadPolies(FS, True);
  ReadNodes(FS);

  w_SetPlaneTypes(m_pNodes, m_pPolies, m_pPlanes, m_nNodes, bUsePlaneTypes);

  //Logger.WLog(LM_WARN, 'WorldData Ends: ' + IntToHex(FS.Position, 8));

end;

procedure TLTWorldBsp.ReadNodes(FS: TMemoryStream);
var i, j, nPoly, nStatus: Cardinal;
    pNode: TLTWorldNode;
    nLeaf: Word;
    anNodeIndices: array[0..1] of Integer = (0, 0);
begin
  if m_nNodes > 0 then
  begin
    nPoly := 0;
    nLeaf := 0;
    nStatus := 0;
    for i := 0 to m_nNodes - 1 do
    begin
      pNode := TLTWorldNode.Create;
      FS.Read(nPoly, 4);
      if nPoly >= m_nPolies then
      begin
        Logger.WLog(LM_WARN, Format('Node %d has invalid poly index (%d)', [i, nPoly]));
      end;
      pNode.Poly := nPoly;
      pNode.Flags := 0;
      pNode.PlaneType := 0;

      FS.Read(nLeaf, 2);
      pNode.Leaf := nLeaf;

      FS.Read(anNodeIndices, 8);

      for j := 0 to 1 do
      begin
        //pNode.Sides[j] := w_NodeForIndex(m_nNodes, anNodeIndices[j], nStatus);
        //pNode.SidesStatus[j] := nStatus;
        pNode.m_anSides[j] := w_NodeForIndex(m_nNodes, anNodeIndices[j], nStatus);
        pNode.m_anSidesStatus[j] := nStatus;
      end;

      m_pNodes.Add(pNode);
    end;
    // not ised in AVP2 ?
    //FS.Read(anNodeIndices[0], 4);
    //m_nRootNode := w_NodeForIndex(m_nNodes, anNodeIndices[0], m_nRootNodeStatus);
  end;

  // test
  FS.Read(m_UnknownStruct{%H-}, SizeOf(TUnknownStruct));
end;

procedure TLTWorldBsp.SaveNodesDump(S: string);
var FS: TMemoryStream;
    i: Cardinal;
    pPoly: TLTWorldPoly;
    pNode: TLTWorldNode;
    pVertex: TLTWorldVertex;
begin
  FS := TMemoryStream.Create; //TFileStream.Create(S, fmOpenReadWrite + fmCreate);
  // header
  FS.Write('LTND', 4);
  FS.Write(m_nPolies, 4);
  FS.Write(m_nNodes, 4);
  FS.Write(m_nPoints, 4);
  // polies
  if m_nPolies > 0 then
  for i := 0 to m_nPolies - 1 do
  begin
    pPoly := TLTWorldPoly(m_pPolies.Items[i]);
    FS.Write(pPoly.GetNumVertices, 4);
    FS.Write(pPoly.Surface, 4);
    FS.Write(pPoly.DiskVerts[0], SizeOf(TLTDiskVert) * pPoly.GetNumVertices);
  end;
  // nodes
  if m_nNodes > 0 then
  for i := 0 to m_nNodes - 1 do
  begin
    pNode := TLTWorldNode(m_pNodes.Items[i]);
    FS.Write(pNode.Poly, 4);
    FS.Write(pNode.Sides[0], 8);
    FS.Write(pNode.SidesStatus[0], 8);
    FS.Write(pNode.Leaf, 2);
  end;
  // poitns
  if m_nPoints > 0 then
  for i := 0 to m_nPoints - 1 do
  begin
    pVertex := TLTWorldVertex(m_pPoints.Items[i]);
    FS.Write(pVertex.m_vData, SizeOf(LTVector));
  end;
  FS.SaveToFile(S);
  FS.Free;
end;

procedure TLTWorldBsp.ReadPoints(FS: TMemoryStream);
var i: Cardinal;
    pVertex: TLTWorldVertex;
begin
  if m_nPoints> 0 then
  for i := 0 to m_nPoints -1 do
  begin
    pVertex := TLTWorldVertex.Create;
    FS.Read(pVertex.m_vData, sizeof(LTVector));
    //WriteLn(LTVectorToStrC(@pVertex.m_vData));
    m_pPoints.Add(pVertex);
  end;
end;

procedure TLTWorldBsp.ReadSurfaces(FS: TMemoryStream);
var i: Cardinal;
    pSurface: TLTWorldSurface;
    nLen: Word;
begin
  // test
  {if m_szWorldName = 'TranslucentWorldModel10' then
  begin
    i := 0;
    WriteLn(IntToHex(FS.Position, 8));
  end;  }

  nLen := 0;

  if m_nSurfaces > 0 then
  for i := 0 to m_nSurfaces - 1 do
  begin
    //WriteLn(IntToHex(FS.Position, 8));
    pSurface := TLTWorldSurface.Create;
    FS.Read(pSurface.m_fUV1, sizeof(LTVector));
    FS.Read(pSurface.m_fUV2, sizeof(LTVector));
    FS.Read(pSurface.m_fUV3, sizeof(LTVector));
    FS.Read(pSurface.m_nTexture, 2);
    FS.Read(pSurface.m_nFlags, 4);
    FS.Read(pSurface.m_nUnknown1, 4);
    FS.Read(pSurface.m_nUseEffect, 1);
    if pSurface.m_nUseEffect > 0 then
    begin
      FS.Read(nLen, 2);
      if nLen > 0 then
      begin
        SetLength(pSurface.m_szEffect, nLen);
        FS.Read(pSurface.m_szEffect[1], nLen);
      end;
      FS.Read(nLen, 2);
      if nLen > 0 then
      begin
        SetLength(pSurface.m_szEffectParam, nLen);
        FS.Read(pSurface.m_szEffectParam[1], nLen);
      end;
    end;
    FS.Read(pSurface.m_nTextureFlags, 2);
    m_pSurfaces.Add(pSurface);
  end;
end;

procedure TLTWorldBsp.ReadPlanes(FS: TMemoryStream);
var i: Cardinal;
    pPlane: TLTWorldPlane;
begin
  if m_nPlanes > 0 then
  for i := 0 to m_nPlanes - 1 do
  begin
    pPlane := TLTWorldPlane.Create;
    FS.Read(pPlane.m_vNormal, sizeof(LTVector));
    FS.Read(pPlane.m_fDist, 4);
    m_pPlanes.Add(pPlane);
  end;
end;

procedure TLTWorldBsp.ReadPolies(FS: TMemoryStream; bReadData: Boolean);
var i: Cardinal;
    nVertices: Word;
    pPoly: TLTWorldPoly;
begin
  if not bReadData then
  begin
    nVertices := 0;
    for i := 0 to m_nPolies - 1 do
    begin
      FS.Read(nVertices, 2);
      pPoly := TLTWorldPoly.Create;
      m_pPolies.Add(pPoly);
      pPoly.SetIndex(i);
      // WTF?
      pPoly.LoVerts := lo(nVertices);
      pPoly.HiVerts := hi(nVertices);
      //if (pPoly.HiVerts > 0) then
         //WriteLn('[WARNING] Poly #', i, ' has additional vertices [', pPoly.HiVerts, ']');
       //  Logger.WLog(LM_WARN, 'Poly #' + IntToStr(i) + ' has additional vertices [' + IntToStr(pPoly.HiVerts) + ']');

      nVertices := pPoly.LoVerts + pPoly.HiVerts;
      pPoly.SetNumVertices(nVertices);
    end;
  end
  else
  begin
    for i := 0 to m_nPolies - 1 do
    begin
      TLTWorldPoly(m_pPolies.Items[i]).ReadPoly(FS);
    end;
  end;
end;

procedure TLTWorldBsp.ReadTextures(FS: TMemoryStream);
var i, j: Cardinal;
begin
  FS.Read(m_nNamesLen, 4);
  FS.Read(m_nTextures, 4);

  if m_nTextures > 0 then
  for i := 0 to m_nTextures - 1 do
  begin
    SetLength(m_aszTextureNames, i+1);
    SetLength(m_aszTextureNames[i], 1);
    j := 1;
    repeat
      FS.Read(m_aszTextureNames[i][1], 1);
      Inc(j, 1);
    until m_aszTextureNames[i][1] = #0;
    Dec(j, 1);
    SetLength(m_aszTextureNames[i], j - 1);
    FS.Position := FS.Position - j;
    FS.Read(m_aszTextureNames[i][1], j - 1);
    FS.Position := FS.Position + 1;
  end;
end;

procedure TLTWorldBsp.ReadLeafs(FS: TMemoryStream);
var i, j: Cardinal;
    nNumLeafLists: Word;
    nPoliesCount: Cardinal = 0;
    nTempWord: Word = 0;
begin
  WriteLn('LeafsTest: ',  IntToHex(FS.Position, 8));
  nNumLeafLists := 0;
  if m_nLeafs > 0 then
  begin
    for i := 0 to m_nLeafs - 1 do
    begin
      FS.Read(nNumLeafLists, 2);
      if nNumLeafLists > 0 then
      for j := 0 to nNumLeafLists-1 do
      begin
        FS.Read(nTempWord, 2);
        FS.Read(nTempWord, 2);
        FS.Position := FS.Position + nTempWord;
      end;
      FS.Read(nPoliesCount, 4);
      FS.Read(nTempWord, 2);
      FS.Position := FS.Position + (Int64(nPoliesCount) * 4);
      FS.Read(nTempWord, 2);
    end;
  end;
end;

constructor TLTWorldBsp.Create;
begin
  m_pPolies := TFPObjectList.Create(True);
  m_pPlanes := TFPObjectList.Create(True);
  m_pSurfaces := TFPObjectList.Create(True);
  m_pPoints := TFPObjectList.Create(True);
  m_pNodes := TFPObjectList.Create(True);
end;

destructor TLTWorldBsp.Destroy;
var i: Cardinal;
begin
  inherited;
  for i := 0 to m_nTextures - 1 do
  begin
    SetLength(m_aszTextureNames[i], 0);
  end;
  SetLength(m_aszTextureNames, 0);
  m_pPolies.Free;
  m_pPlanes.Free;
  m_pSurfaces.Free;
  m_pPoints.Free;
  m_pNodes.Free;
end;

constructor TLTWorldPoly.Create;
begin

end;

destructor TLTWorldPoly.Destroy;
begin
  inherited;
  SetLength(m_anUnknownList, 0);
end;

function w_NodeForIndex(nListSize: Cardinal; nIndex: Integer; var nStatus: Cardinal): Cardinal;
begin
  Result := 0;
  if nIndex = -1 then
  begin
    nStatus := NFI_NODE_IN;
  end
  else if nIndex = -2 then
  begin
    nStatus := NFI_NODE_OUT;
  end
  else if nIndex >= Integer(nListSize) then
  begin
    nStatus := NFI_ERROR;
  end
  else
  begin
    Result := nIndex;
    nStatus := NFI_OK;
  end;
end;

procedure w_SetPlaneTypes(pNodeList: TFPObjectList; pPolyList: TFPObjectList; pPlaneList: TFPObjectList; nNodes: Cardinal; bUsePlaneTypes: Boolean);
var i: Cardinal;
    pNode: TLTWorldNode;
    pPlane: TLTWorldPlane;
    pPoly: TLTWorldPoly;
begin
  if nNodes > 0 then
  for i := 0 to nNodes - 1 do
  begin
    pNode := TLTWorldNode(pNodeList.Items[i]);
    pNode.PlaneType := PLANE_GENERIC;
    if bUsePlaneTypes then
    begin
      pPoly := TLTWorldPoly(pPolyList.Items[pNode.Poly]);
      pPlane := TLTWorldPlane(pPlaneList.Items[pPoly.Plane]);

      if pPlane.m_vNormal.x > PLANE_EP then pNode.PlaneType := PLANE_POSX
      else if pPlane.m_vNormal.x < -PLANE_EP then pNode.PlaneType := PLANE_NEGX
      else if pPlane.m_vNormal.y > PLANE_EP then pNode.PlaneType := PLANE_POSY
      else if pPlane.m_vNormal.y < -PLANE_EP then pNode.PlaneType := PLANE_NEGY
      else if pPlane.m_vNormal.z > PLANE_EP then pNode.PlaneType := PLANE_POSZ
      else if pPlane.m_vNormal.z < -PLANE_EP then pNode.PlaneType := PLANE_NEGZ;
    end;
  end;
end;

end.

