unit ltworlddata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ltworldtypes, contnrs, globals, MyLogger;

const
  LANDSCAPE_LIGHTMAPS_PER_LINE = 60;

type

  TLTWorldSurface = class(TObject)
  public
    m_fUV1: LTVector;
    m_fUV2: LTVector;
    m_fUV3: LTVector;
    m_vUnknown1: LTVector;
    m_vUnknown2: LTVector;
    m_vUnknown3: LTVector;
    m_nTexture: Word;
    m_nPlane: Cardinal;
    m_nFlags: Cardinal;
    m_nUnknown1: Byte;
    m_nUnknown2: Byte;
    m_nUnknown3: Byte;
    m_nUnknown4: Byte;
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

  TLTRelDiskVert = packed record
    nRelVerts: Word;
  end;

  TLTDiskVert = packed record
    nVerts: Word;
    nDummy1: Byte;
    nDummy2: Byte;
    nDummy3: Byte;
  end;
  TLTDiskVertList = array[0..MAX_WORLDPOLY_VERTS - 1] of TLTDiskVert;
  TLTRelDiskVertList = array[0..MAX_WORLDPOLY_VERTS - 1] of TLTRelDiskVert;
  TLTNodeIndices = array[0..1] of Cardinal;

  TLTWorldNode = class(TObject)
  private
    m_nPlaneType: Byte;
    m_nUnknown1: Cardinal;
    m_nPoly: Cardinal;
    m_nLeaf: Word;
    m_fUnknown1: LTFloat;
    m_fUnknown2: LTFloat;
    m_fUnknown3: LTFloat;
    m_fUnknown4: LTFloat;
  public
    m_anSides: TLTNodeIndices;
    m_anSidesStatus: TLTNodeIndices;

    property PlaneType: Byte read m_nPlaneType write m_nPlaneType;
    property Leaf: Word read m_nLeaf write m_nLeaf;
    property Poly: Cardinal read m_nPoly write m_nPoly;
    property UnknownCardinal1: Cardinal read m_nUnknown1 write m_nUnknown1;
    property UnknownFloat1: LTFloat read m_fUnknown1 write m_fUnknown1;
    property UnknownFloat2: LTFloat read m_fUnknown2 write m_fUnknown2;
    property UnknownFloat3: LTFloat read m_fUnknown3 write m_fUnknown3;
    property UnknownFloat4: LTFloat read m_fUnknown4 write m_fUnknown4;
    property Sides: TLTNodeIndices read m_anSides write m_anSides;
    property SidesStatus: TLTNodeIndices read m_anSidesStatus write m_anSidesStatus;
  end;

  { TLTWorldPoly }

  TLTWorldPoly = class(TObject)
  private
    m_nIndexAndNumVerts: Cardinal;

    m_nLoVerts: Byte;
    m_nHiVerts: Byte;

    m_nLightmapWidth: Word;
    m_nLightmapHeight: Word;

    m_vUV1: LTVector;
    m_vUV2: LTVector;
    m_vUV3: LTVector;

    m_nUnknown1: Cardinal;
    m_nUnknown2: Cardinal;
    //m_nPlane: Cardinal;
    m_nSurface: Cardinal;

    m_aDiskVerts: TLTDiskVertList;
    m_aRelDiskVerts: TLTRelDiskVertList;

    //m_nLMFrameIndex: Cardinal;

    procedure FillRelVerts;
  public
    property LightmapWidth: Word read m_nLightmapWidth write m_nLightmapWidth;
    property LightmapHeight: Word read m_nLightmapHeight write m_nLightmapHeight;

    property LoVerts: Byte read m_nLoVerts write m_nLoVerts;
    property HiVerts: Byte read m_nHiVerts write m_nHiVerts;

    property UVData1: LTVector read m_vUV1 write m_vUV1;
    property UVData2: LTVector read m_vUV2 write m_vUV2;
    property UVData3: LTVector read m_vUV3 write m_vUV3;
    property DiskVerts: TLTDiskVertList read m_aDiskVerts write m_aDiskVerts;
    property RelDiskVerts: TLTRelDiskVertList read m_aRelDiskVerts write m_aRelDiskVerts;

    //property LMFrameIndex: Cardinal read m_nLMFrameIndex write m_nLMFrameIndex;

    property IndexAndNumVerts: Cardinal read m_nIndexAndNumVerts write m_nIndexAndNumVerts;

    property UnknownCardinal1: Cardinal read m_nUnknown1 write m_nUnknown1;
    property UnknownCardinal2: Cardinal read m_nUnknown2 write m_nUnknown2;

    //property Plane: Cardinal read m_nPlane write m_nPlane;
    property Surface: Cardinal read m_nSurface write m_nSurface;
    function GetNumVertices: Cardinal;
    procedure SetNumVertices(nVertices: Cardinal);
    function GetIndex: Cardinal;
    procedure SetIndex(nIndex: Cardinal);
    procedure ReadPoly(FS: TMemoryStream);

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLTPBlockRecord }

  TLTPBlockRecord = class(TObject)
  public
    m_nSize: Word;
    m_nWord1: Word;
    m_pContents: array of Byte;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLTPBlockTable }

  TLTPBlockTable = class(TObject)
  public
    m_nCardinal1: Cardinal;
    m_nCardinal2: Cardinal;
    m_nCardinal3: Cardinal;
    m_nSize: Cardinal;
    m_vVector1: LTVector;
    m_vVector2: LTVector;
    m_pRecords: TFPObjectList;
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

    m_pPolies: TFPObjectList;
    m_pPlanes: TFPObjectList;
    m_pSurfaces: TFPObjectList;
    m_pPoints: TFPObjectList;
    m_pLeafs: TFPObjectList;
    m_pNodes: TFPObjectList;
    m_pUserPortals: TFPObjectList;
    m_pPBlockTable: TLTPBlockTable;

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

    property MinBox: LTVector read m_vMinBox write m_vMinBox;
    property MaxBox: LTVector read m_vMaxBox write m_vMaxBox;
    property WorldTranslation: LTVector read m_vWorldTranslation write m_vWorldTranslation;

    property TextureNames: TStringArray read m_aszTextureNames write m_aszTextureNames;

    property PoliesList: TFPObjectList read m_pPolies write m_pPolies;
    property PlanesList: TFPObjectList read m_pPlanes write m_pPlanes;
    property SurfacesList: TFPObjectList read m_pSurfaces write m_pSurfaces;
    property PointsList: TFPObjectList read m_pPoints write m_pPoints;
    property LeafsList: TFPObjectList read m_pLeafs write m_pLeafs;
    property NodesList: TFPObjectList read m_pNodes write m_pNodes;
    property UserPortalList: TFPObjectList read m_pUserPortals write m_pUserPortals;
    property PBlockTable: TLTPBlockTable read m_pPBlockTable write m_pPBlockTable;

    function Load(FS: TMemoryStream; {%H-}bUsePlaneTypes: Boolean): Integer;
    procedure ReadTextures(FS: TMemoryStream);
    procedure ReadPolies(FS: TMemoryStream; bReadData: Boolean);
    procedure ReadLeafsJP(FS: TMemoryStream);
    procedure ReadLeafs(FS: TMemoryStream);
    procedure ReadPlanes(FS: TMemoryStream);
    procedure ReadSurfaces(FS: TMemoryStream);
    procedure ReadPoints(FS: TMemoryStream);
    procedure ReadNodesJP(FS: TMemoryStream);
    procedure ReadNodes(FS: TMemoryStream);
    procedure ReadRootNode(FS: TMemoryStream);
    procedure ReadUserPortals(FS: TMemoryStream);
    procedure ReadPBlockTable(FS: TMemoryStream);

    procedure CopyUVsToPolies;
    procedure SaveNodesDump(S: string);

    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TLTWorldData = class(TObject)
  private
    m_nFlags: Cardinal;
    m_pOriginalBSP: TLTWorldBsp;
    m_nNextPos: Cardinal;
    m_nRenderDataPos: Cardinal;
  protected
  public
    property Flags: Cardinal read m_nFlags write m_nFlags;
    property OriginalBSP: TLTWorldBsp read m_pOriginalBSP write m_pOriginalBSP;
    property NextPos: Cardinal read m_nNextPos write m_nNextPos;
    property RenderDataPos: Cardinal read m_nRenderDataPos write m_nRenderDataPos;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLMPolyDataSH }

  TLMPolyDataSH = class(TObject)
  public
    m_vUnknown: LTVector;
    m_nLMWidth: Byte;
    m_nLMHeight: Byte;
    m_nLandscapeX: Word;
    m_nLandscapeY: Word;
    m_nLMSize: Integer;
    m_anLMData: TDynByteArray;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLMAnimSH }

  TLMAnimSH = class(TObject)
    private
      m_szName: string;
      m_pPolyDataList: TFPObjectList;
      procedure CreateLandscape(var Buffer: TDynByteArray; var nFullWidth: Word;
        var nFullHeight: Word);
    public
      m_nUnknown1: Cardinal;
      m_nUnknown2: Cardinal;
      property Name: string read m_szName write m_szName;
      property PolyDataList: TFPObjectList read m_pPolyDataList write m_pPolyDataList;
      procedure SaveOnDisk;
      constructor Create; virtual;
      destructor Destroy; override;
  end;

  { TLTUserPortal }

  TLTUserPortal = class(TObject)
  public
    m_szName: string;
    m_nCardinal1: Cardinal;
    m_nWord1: Word;
    m_vCenter: LTVector;
    m_vDims: LTVector;
  end;

  { TLTLeafList }

  TLTLeafList = class(TObject)
  public
    m_nPortalId: Word;
    m_nSize: Word;
    m_pContents: array of Byte;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLTLeaf }

  TLTLeaf = class(TObject)
  public
    m_nNumLeafLists: Word;
    m_nLeafListIndex: Word;
    m_pLeafLists: TFPObjectList;
    m_nPoliesCount: Word;
    m_pPolies: array of Word;
    m_fFloat1: LTFloat;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

function w_NodeForIndex(nListSize: Cardinal; nIndex: Integer; var nStatus: Cardinal): Cardinal;
procedure w_SetPlaneTypes(pNodeList: TFPObjectList; pPolyList: TFPObjectList; pPlaneList: TFPObjectList; nNodes: Cardinal; bUsePlaneTypes: Boolean);

implementation

{ TLMPolyDataSH }

constructor TLMPolyDataSH.Create;
begin

end;

destructor TLMPolyDataSH.Destroy;
begin
  SetLength(m_anLMData, 0);
  inherited Destroy;
end;

{ TLMAnimSH }

procedure TLMAnimSH.CreateLandscape(var Buffer: TDynByteArray; var nFullWidth: Word; var nFullHeight: Word);
var i: Cardinal;
    pData: TLMPolyDataSH;
    nLineWidth, nLineHeight: Word;
    nLineCounter: Cardinal = 0;
    bFinalLine: Boolean = False;
begin
  nLineWidth := 0;
  nLineHeight := 0;

  for i := 0 to m_pPolyDataList.Count - 1 do
  begin
    pData := TLMPolyDataSH(m_pPolyDataList.Items[i]);

    if pData.m_nLMSize = 0 then
      Continue;

    if nLineCounter < LANDSCAPE_LIGHTMAPS_PER_LINE then
    begin
      Inc(nLineWidth, pData.m_nLMWidth);

      if pData.m_nLMHeight > nLineHeight then
        nLineHeight := pData.m_nLMHeight;

      Inc(nLineCounter, 1);

      pData.m_nLandscapeX := nLineWidth - pData.m_nLMWidth;
      pData.m_nLandscapeY := nFullHeight;

      bFinalLine := True;
    end
    else
    begin
      Inc(nLineWidth, pData.m_nLMWidth);

      if pData.m_nLMHeight > nLineHeight then
        nLineHeight := pData.m_nLMHeight;

      pData.m_nLandscapeX := nLineWidth - pData.m_nLMWidth;
      pData.m_nLandscapeY := nFullHeight;

      if nLineWidth > nFullWidth then
        nFullWidth := nLineWidth;
      Inc(nFullHeight, nLineHeight);

      nLineCounter := 0;
      nLineWidth := 0;
      nLineHeight := 0;

      bFinalLine := False;
    end;

  end;

  if bFinalLine then
  begin
    if nLineWidth > nFullWidth then
      nFullWidth := nLineWidth;
    Inc(nFullHeight, nLineHeight);
  end;

  SetLength(Buffer, nFullWidth * nFullHeight * 2);

  for i := 0 to m_pPolyDataList.Count - 1 do
  begin
    pData := TLMPolyDataSH(m_pPolyDataList.Items[i]);

    if pData.m_nLMSize = 0 then
      Continue;

    SimpleBlt16(TDynWordArray(Buffer), TDynWordArray(pData.m_anLMData),
      nFullWidth, pData.m_nLMWidth, pData.m_nLMHeight, pData.m_nLandscapeX, pData.m_nLandscapeY);

  end;
end;

procedure TLMAnimSH.SaveOnDisk;
var anBuffer: TDynByteArray;
    nWidth: Word = 0;
    nHeight: Word = 0;
    MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;

  CreateLandscape(anBuffer{%H-}, nWidth, nHeight);
  if nWidth * nHeight > 0 then
    SaveArrayToTGA(anBuffer, nWidth, nHeight, g_szDumpsDir + g_szPathSep + m_szName + '.tga', 4, True, False);

  SetLength(anBuffer, 0);
  MS.Free;
end;

constructor TLMAnimSH.Create;
begin
  m_pPolyDataList := TFPObjectList.Create(True);
end;

destructor TLMAnimSH.Destroy;
begin
  m_pPolyDataList.Free;
  inherited Destroy;
end;

{ TLTLeaf }

constructor TLTLeaf.Create;
begin
  m_nLeafListIndex := 0;
  m_pLeafLists := TFPObjectList.Create(True);
end;

destructor TLTLeaf.Destroy;
begin
  m_pLeafLists.Free;
  SetLength(m_pPolies, 0);
  inherited Destroy;
end;

{ TLTLeafList }

constructor TLTLeafList.Create;
begin

end;

destructor TLTLeafList.Destroy;
begin
  SetLength(m_pContents, 0);
  inherited Destroy;
end;

{ TLTPBlockTable }

constructor TLTPBlockTable.Create;
begin
  m_pRecords := TFPObjectList.Create(True);
end;

destructor TLTPBlockTable.Destroy;
begin
  m_pRecords.Free;
  inherited Destroy;
end;

{ TLTPBlockRecord }

constructor TLTPBlockRecord.Create;
begin

end;

destructor TLTPBlockRecord.Destroy;
begin
  SetLength(m_pContents, 0);
  inherited Destroy;
end;

procedure TLTWorldPoly.ReadPoly(FS: TMemoryStream);
begin

  FS.Read(m_nLightmapWidth, 2);
  FS.Read(m_nLightmapHeight, 2);

  FS.Read(m_nUnknown1, 4);
  FS.Read(m_nUnknown2, 4);
  FS.Read(m_nSurface, 4);

  FS.Read(m_aDiskVerts[0], SizeOf(TLTDiskVert) * GetNumVertices);
  FillRelVerts;
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

procedure TLTWorldPoly.FillRelVerts;
var i: Cardinal;
begin
  for i := 0 to m_nLoVerts - 1 do
  begin
    m_aRelDiskVerts[i].nRelVerts := i;
  end;
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
var dwWorldInfoFlags,
  dwUnknown1, dwUnknown2, dwUnknown3, dwUnknown4, dwUnknown5: Cardinal;
    nNameLen: Word;
    nTemp: TEventType;
    //pVertex: TLTWorldVertex;
begin
  Result := 0;
  dwWorldInfoFlags := 0;
  dwUnknown1 := 0;
  dwUnknown2 := 0;
  dwUnknown3 := 0;
  dwUnknown4 := 0;
  dwUnknown5 := 0;
  nNameLen := 0;
  FS.Read(dwWorldInfoFlags, 4);
  m_nWorldInfoFlags := dwWorldInfoFlags;
  FS.Read(nNameLen, 2);
  SetLength(m_szWorldName, nNameLen);
  if nNameLen > 0 then
    FS.Read(m_szWorldName[1], nNameLen);

  WriteLn('--- Loading BSP: ', m_szWorldName);
  nTemp := Logger.RootLevel;
  Logger.RootLevel := etInfo;
  WLogStr('--- Loading BSP: ' + m_szWorldName);
  Logger.RootLevel := nTemp;

  FS.Read(dwUnknown1, 4);
  FS.Read(m_nPoints, 4);
  FS.Read(m_nPlanes, 4);
  FS.Read(m_nSurfaces, 4);
  FS.Read(m_nUserPortals, 4);
  FS.Read(m_nPolies, 4);
  FS.Read(m_nLeafs, 4);
  FS.Read(dwUnknown2, 4);
  FS.Read(dwUnknown3, 4);
  FS.Read(dwUnknown4, 4);
  FS.Read(m_nNodes, 4);
  FS.Read(dwUnknown5, 4);

  FS.Read(m_vMinBox, SizeOf(LTVector));
  FS.Read(m_vMaxBox, SizeOf(LTVector));
  FS.Read(m_vWorldTranslation, SizeOf(LTVector));

  FS.Read(m_nNamesLen, 4);
  FS.Read(m_nTextures, 4);

  ReadTextures(FS);
  ReadPolies(FS, False);

  ReadLeafs(FS);

  ReadPlanes(FS);

  ReadSurfaces(FS);

  ReadPolies(FS, True);

  ReadNodes(FS);

  ReadUserPortals(FS);

  ReadPoints(FS);

  ReadPBlockTable(FS);
  ReadRootNode(FS);

  CopyUVsToPolies;

  //w_SetPlaneTypes(m_pNodes, m_pPolies, m_pPlanes, m_nNodes, bUsePlaneTypes);

  //Logger.WLog(LM_WARN, 'WorldData Ends: ' + IntToHex(FS.Position, 8));

end;

procedure TLTWorldBsp.ReadNodesJP(FS: TMemoryStream);
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
        WLogStrWarn(Format('Node %d has invalid poly index (%d)', [i, nPoly]));
      end;
      pNode.Poly := nPoly;
      pNode.PlaneType := 0;

      FS.Read(nLeaf, 2);
      pNode.Leaf := nLeaf;

      FS.Read(anNodeIndices, 8);

      for j := 0 to 1 do
      begin
        pNode.m_anSides[j] := w_NodeForIndex(m_nNodes, anNodeIndices[j], nStatus);
        pNode.m_anSidesStatus[j] := nStatus;
      end;

      m_pNodes.Add(pNode);
    end;
  end;
end;

procedure TLTWorldBsp.ReadNodes(FS: TMemoryStream);
var i, j, nPoly, nUnknown1, nStatus: Cardinal;
    afUnknown: array[0..3] of LTFloat = (0, 0, 0, 0);
    pNode: TLTWorldNode;
    nLeaf: Word;
    nNodeIndex: Integer;
begin
  if m_nNodes > 0 then
  begin

    nPoly := 0;
    nLeaf := 0;
    nNodeIndex := 0;
    nStatus := 0;
    nUnknown1 := 0;
    for i := 0 to m_nNodes - 1 do
    begin
      pNode := TLTWorldNode.Create;
      FS.Read(nUnknown1, 4);
      FS.Read(nPoly, 4);

      {if nPoly >= m_nPolies then
      begin
        WLogStrWarn(Format('Node %d has invalid poly index (%d)', [i, nPoly]));
        Exit;
      end; }
      pNode.UnknownCardinal1 := nUnknown1;
      pNode.Poly := nPoly;

      FS.Read(nLeaf, 2);
      pNode.Leaf := nLeaf;

      for j := 0 to 1 do
      begin
        FS.Read(nNodeIndex, 4);
        pNode.m_anSides[j] := w_NodeForIndex(m_nNodes, nNodeIndex, nStatus);
        pNode.m_anSidesStatus[j] := nStatus;
      end;

      FS.Read(afUnknown[0], SizeOf(LTFloat) * 4);
      pNode.m_fUnknown1 := afUnknown[0];
      pNode.m_fUnknown2 := afUnknown[1];
      pNode.m_fUnknown3 := afUnknown[2];
      pNode.m_fUnknown4 := afUnknown[3];

      m_pNodes.Add(pNode);
    end;
  end;
end;

procedure TLTWorldBsp.ReadRootNode(FS: TMemoryStream);
var pNode: TLTWorldNode;
    nStatus: Cardinal;
    nNodeIndex: Integer;
begin
  nNodeIndex := 0;
  nStatus := 0;

  pNode := TLTWorldNode.Create;

  FS.Read(nNodeIndex, 4);
  pNode.m_anSides[0] := w_NodeForIndex(m_nNodes, nNodeIndex, nStatus);
  pNode.m_anSidesStatus[0] := nStatus;

  m_pNodes.Add(pNode);
end;

procedure TLTWorldBsp.ReadUserPortals(FS: TMemoryStream);
var i: Cardinal;
    nNameLen: Word = 0;
    pUserPortal: TLTUserPortal;
begin
  if m_nUserPortals > 0 then
  begin
    for i := 0 to m_nUserPortals - 1 do
    begin
      pUserPortal := TLTUserPortal.Create;

      FS.Read(nNameLen, 2);
      SetLength(pUserPortal.m_szName, nNameLen);
      FS.Read(pUserPortal.m_szName[1], nNameLen);

      FS.Read(pUserPortal.m_nCardinal1, 4);
      //FS.Read(pUserPortal.m_nCardinal2, 4);
      FS.Read(pUserPortal.m_nWord1, 2);
      FS.Read(pUserPortal.m_vCenter, SizeOf(LTVector));
      FS.Read(pUserPortal.m_vDims, SizeOf(LTVector));

      m_pUserPortals.Add(pUserPortal);
    end;
  end;
end;

procedure TLTWorldBsp.ReadPBlockTable(FS: TMemoryStream);
var i: Cardinal;
    pRecord: TLTPBlockRecord;
begin

  FS.Read(m_pPBlockTable.m_nCardinal1, 4);
  FS.Read(m_pPBlockTable.m_nCardinal2, 4);
  FS.Read(m_pPBlockTable.m_nCardinal3, 4);

  m_pPBlockTable.m_nSize := m_pPBlockTable.m_nCardinal1 * m_pPBlockTable.m_nCardinal2 * m_pPBlockTable.m_nCardinal3;

  FS.Read(m_pPBlockTable.m_vVector1{%H-}, SizeOf(LTVector));
  FS.Read(m_pPBlockTable.m_vVector2, SizeOf(LTVector));

  if m_pPBlockTable.m_nSize > 0 then
  begin
    for i := 0 to m_pPBlockTable.m_nSize - 1 do
    begin
      pRecord := TLTPBlockRecord.Create;
      FS.Read(pRecord.m_nSize, 2);
      FS.Read(pRecord.m_nWord1, 2);

      if pRecord.m_nSize > 0 then
      begin
        SetLength(pRecord.m_pContents, 6 * pRecord.m_nSize);
        FS.Read(pRecord.m_pContents[0], 6 * pRecord.m_nSize);
      end;

      m_pPBlockTable.m_pRecords.Add(pRecord);
    end;
  end;

end;

procedure TLTWorldBsp.CopyUVsToPolies;
var i: Cardinal;
    pPoly: TLTWorldPoly;
    pSurface: TLTWorldSurface;
begin
  for i := 0 to m_nPolies - 1 do
  begin
    pPoly := TLTWorldPoly(m_pPolies.Items[i]);
    pSurface := TLTWorldSurface(m_pSurfaces.Items[pPoly.Surface]);
    pPoly.UVData1 := pSurface.m_fUV1;
    pPoly.UVData2 := pSurface.m_fUV2;
    pPoly.UVData3 := pSurface.m_fUV3;
  end;
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

  nLen := 0;

  if m_nSurfaces > 0 then
  for i := 0 to m_nSurfaces - 1 do
  begin
    //WriteLn(IntToHex(FS.Position, 8));
    pSurface := TLTWorldSurface.Create;
    FS.Read(pSurface.m_fUV1, sizeof(LTVector));
    FS.Read(pSurface.m_fUV2, sizeof(LTVector));
    FS.Read(pSurface.m_fUV3, sizeof(LTVector));
    FS.Read(pSurface.m_vUnknown1, sizeof(LTVector));
    FS.Read(pSurface.m_vUnknown2, sizeof(LTVector));
    FS.Read(pSurface.m_vUnknown3, sizeof(LTVector));
    FS.Read(pSurface.m_nTexture, 2);
    FS.Read(pSurface.m_nPlane, 4);
    FS.Read(pSurface.m_nFlags, 4);
    FS.Read(pSurface.m_nUnknown1, 1);
    FS.Read(pSurface.m_nUnknown2, 1);
    FS.Read(pSurface.m_nUnknown3, 1);
    FS.Read(pSurface.m_nUnknown4, 1);
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
    if j - 1 > 0 then
    begin
      FS.Read(m_aszTextureNames[i][1], j - 1);
    end
    else
    begin
      WLogStrWarn('Texture #' + IntToStr(i) + ' has zero length!');
    end;
    FS.Position := FS.Position + 1;
  end;
end;

procedure TLTWorldBsp.ReadLeafsJP(FS: TMemoryStream);
var i, j: Cardinal;
    nNumLeafLists: Word;
    nPoliesCount: Cardinal = 0;
    nTempWord: Word = 0;
begin
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
      FS.Position := FS.Position {%H-}+ (nPoliesCount * 4);
      FS.Read(nTempWord, 2);
    end;
  end;
end;

procedure TLTWorldBsp.ReadLeafs(FS: TMemoryStream);
var i, j: Cardinal;
    pLeaf: TLTLeaf;
    pLeafList: TLTLeafList;
begin
  if m_nLeafs > 0 then
  begin
    for i := 0 to m_nLeafs - 1 do
    begin
      pLeaf := TLTLeaf.Create;

      FS.Read(pLeaf.m_nNumLeafLists, 2);
      if pLeaf.m_nNumLeafLists = $FFFF then
      begin
        FS.Read(pLeaf.m_nLeafListIndex, 2);
      end
      else
      begin
        if pLeaf.m_nNumLeafLists > 0 then
        begin
          for j := 0 to pLeaf.m_nNumLeafLists - 1 do
          begin
            pLeafList := TLTLeafList.Create;

            FS.Read(pLeafList.m_nPortalId, 2);
            FS.Read(pLeafList.m_nSize, 2);
            SetLength(pLeafList.m_pContents, pLeafList.m_nSize);
            FS.Read(pLeafList.m_pContents[0], pLeafList.m_nSize);

            pLeaf.m_pLeafLists.Add(pLeafList);
          end;
        end;
      end;
      //FS.Read(pLeaf.m_nPoliesCount, 4);
      FS.Read(pLeaf.m_nPoliesCount, 2);
      if pLeaf.m_nPoliesCount > 0 then
      begin
        SetLength(pLeaf.m_pPolies, pLeaf.m_nPoliesCount * 4);
        FS.Read(pLeaf.m_pPolies[0], pLeaf.m_nPoliesCount * 4);
      end;
      FS.Read(pLeaf.m_fFloat1, 4);

      m_pLeafs.Add(pLeaf);
    end;
  end;
end;

constructor TLTWorldBsp.Create;
begin
  m_pPolies := TFPObjectList.Create(True);
  m_pPlanes := TFPObjectList.Create(True);
  m_pSurfaces := TFPObjectList.Create(True);
  m_pPoints := TFPObjectList.Create(True);
  m_pLeafs := TFPObjectList.Create(True);
  m_pNodes := TFPObjectList.Create(True);
  m_pUserPortals := TFPObjectList.Create(True);
  m_pPBlockTable := TLTPBlockTable.Create;
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
  m_pLeafs.Free;
  m_pNodes.Free;
  m_pUserPortals.Free;
  m_pPBlockTable.Free;
end;

constructor TLTWorldPoly.Create;
begin

end;

destructor TLTWorldPoly.Destroy;
begin
  inherited;
  //SetLength(m_anUnknownList, 0);
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
      {pPlane := TLTWorldPlane(pPlaneList.Items[pPoly.Plane]);

      if pPlane.m_vNormal.x > PLANE_EP then pNode.PlaneType := PLANE_POSX
      else if pPlane.m_vNormal.x < -PLANE_EP then pNode.PlaneType := PLANE_NEGX
      else if pPlane.m_vNormal.y > PLANE_EP then pNode.PlaneType := PLANE_POSY
      else if pPlane.m_vNormal.y < -PLANE_EP then pNode.PlaneType := PLANE_NEGY
      else if pPlane.m_vNormal.z > PLANE_EP then pNode.PlaneType := PLANE_POSZ
      else if pPlane.m_vNormal.z < -PLANE_EP then pNode.PlaneType := PLANE_NEGZ; }
    end;
  end;
end;

end.

