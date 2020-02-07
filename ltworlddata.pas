unit ltworlddata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ltworldtypes, contnrs, globals, MyLogger;

const
  LANDSCAPE_LIGHTMAPS_PER_LINE = 60;

type

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

  TLTDiskVert = packed record
    nVerts: Word;
    nDummy1: Byte;
    nDummy2: Byte;
    nDummy3: Byte;
  end;
  TLTDiskVertList = array[0..MAX_WORLDPOLY_VERTS - 1] of TLTDiskVert;
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

    m_nLightmapWidth: Word;
    m_nLightmapHeight: Word;

    m_nUnknownNum: Word;
    m_anUnknownList: TDynWordArray;

    m_fUV1: LTVector;
    m_fUV2: LTVector;
    m_fUV3: LTVector;

    m_nPlane: Cardinal;
    m_nSurface: Cardinal;

    m_aDiskVerts: TLTDiskVertList;
  public
    property Radius: LTFloat read m_fRadius write m_fRadius;
    property Center: LTVector read m_vCenter write m_vCenter;
    property LightmapWidth: Word read m_nLightmapWidth write m_nLightmapWidth;
    property LightmapHeight: Word read m_nLightmapHeight write m_nLightmapHeight;
    property UnknownNum: Word read m_nUnknownNum write m_nUnknownNum;
    property UnknownList: TDynWordArray read m_anUnknownList write m_anUnknownList;

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
    m_nSections: Cardinal;
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

    property MinBox: LTVector read m_vMinBox write m_vMinBox;
    property MaxBox: LTVector read m_vMaxBox write m_vMaxBox;
    property WorldTranslation: LTVector read m_vWorldTranslation write m_vWorldTranslation;

    property TextureNames: TStringArray read m_aszTextureNames write m_aszTextureNames;

    property PoliesList: TFPObjectList read m_pPolies write m_pPolies;
    property PlanesList: TFPObjectList read m_pPlanes write m_pPlanes;
    property SurfacesList: TFPObjectList read m_pSurfaces write m_pSurfaces;
    property PointsList: TFPObjectList read m_pPoints write m_pPoints;
    property NodesList: TFPObjectList read m_pNodes write m_pNodes;

    function Load(FS: TMemoryStream; bUsePlaneTypes: Boolean): Integer;
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

  { TLMFramePolyData }

  TLMFramePolyData = class(TObject)
  public
    m_nVertices: Byte;
    m_anData: TDynByteArray;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLMFrame }

  TLMFrame = class(TObject)
  public
    m_nSize: Word;
    m_nDecSize: Word;
    m_nReSize: Word;
    m_nWidth: Word;
    m_nHeight: Word;
    m_nLandscapeX: Word;
    m_nLandscapeY: Word;
    m_anData: TDynByteArray;
    m_anDecData: TDynByteArray;
    m_anReData: TDynByteArray;
    procedure SwapRB;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TLMBatch }

  TLMBatch = class(TObject)
  private
    m_pFrames: TFPObjectList;
    m_pFramePolyData: TFPObjectList;
  public
    property FramesList: TFPObjectList read m_pFrames write m_pFrames;
    property FramePolyDataList: TFPObjectList read m_pFramePolyData write m_pFramePolyData;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TLMPolyRef = class(TObject)
  public
    m_nModelIndex: Word;
    m_nPolyIndex: Word;
    m_pWorldPoly: TLTWorldPoly;
  end;

  { TLMAnim }

  TLMAnim = class(TObject)
  private
    m_szName: string;
    m_nLMType: Cardinal;
    m_nBatches: Byte;
    m_nFrames: Word;
    m_pPolyRefs: TFPObjectList;
    m_pBatches: TFPObjectList;
    procedure CreateFramesLandscape(var Buffer: TDynByteArray; pBatch: TLMBatch; var nFullWidth: Word; var nFullHeight: Word; MS: TMemoryStream);
    procedure CreatePolyDataLandscape(var Buffer: TDynByteArray; pBatch: TLMBatch; var nFullWidth: Word; var nFullHeight: Word; MS: TMemoryStream);
    procedure LoadFramesFromStream(pBatch: TLMBatch; MS: TMemoryStream; anBuffer: TDynByteArray; nFullWidth: Word);
    procedure LoadPolyDataFromStream(pBatch: TLMBatch; MS: TMemoryStream; anBuffer: TDynByteArray);
  public
    property Name: string read m_szName write m_szName;
    property LMType: Cardinal read m_nLMType write m_nLMType;
    property Batches: Byte read m_nBatches write m_nBatches;
    property Frames: Word read m_nFrames write m_nFrames;
    property PolyRefsList: TFPObjectList read m_pPolyRefs write m_pPolyRefs;
    property BatchesList: TFPObjectList read m_pBatches write m_pBatches;
    procedure SaveBatchesOnDisk;
    procedure LoadBatchesFromDisk;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

function w_NodeForIndex(nListSize: Cardinal; nIndex: Integer; var nStatus: Cardinal): Cardinal;
procedure w_SetPlaneTypes(pNodeList: TFPObjectList; pPolyList: TFPObjectList; pPlaneList: TFPObjectList; nNodes: Cardinal; bUsePlaneTypes: Boolean);

implementation

{ TLMFramePolyData }

constructor TLMFramePolyData.Create;
begin
  m_nVertices := 0;
end;

destructor TLMFramePolyData.Destroy;
begin
  inherited Destroy;
  if m_nVertices > 0 then
    SetLength(m_anData, 0);
end;

{ TLMFrame }

procedure TLMFrame.SwapRB;
var i: Integer;
    nTemp: Byte;
begin
  i := 0;
  while i < Length(m_anDecData) do
  begin
    nTemp := m_anDecData[i];
    m_anDecData[i] := m_anDecData[i + 2];
    m_anDecData[i + 2] := nTemp;
    if m_anDecData[i + 3] <> 0 then
      Logger.WLog(LM_WARN, 'LM alpha is not zero for some reason!');
    Inc(i, 4)
  end;
end;

constructor TLMFrame.Create;
begin
  m_nSize := 0;
  m_nDecSize := 0;
  m_nReSize := 0;
end;

destructor TLMFrame.Destroy;
begin
  inherited Destroy;
  if m_nSize > 0 then SetLength(m_anData, 0);
  if m_nDecSize > 0 then SetLength(m_anDecData, 0);
  if m_nReSize > 0 then SetLength(m_anReData, 0);
end;

{ TLMBatch }

constructor TLMBatch.Create;
begin
  m_pFrames := TFPObjectList.Create(True);
  m_pFramePolyData := TFPObjectList.Create(True);
end;

destructor TLMBatch.Destroy;
begin
  inherited Destroy;
  m_pFrames.Free;
  m_pFramePolyData.Free;
end;

{ TLMAnim }

procedure TLMAnim.CreateFramesLandscape(var Buffer: TDynByteArray; pBatch: TLMBatch; var nFullWidth: Word; var nFullHeight: Word; MS: TMemoryStream);
var i: Cardinal;
    pFrame: TLMFrame;
    nLineWidth, nLineHeight: Word;
    nLineCounter: Cardinal = 0;
    bFinalLine: Boolean = False;
begin
  nLineWidth := 0;
  nLineHeight := 0;

  for i := 0 to m_nFrames - 1 do
  begin
    pFrame := TLMFrame(pBatch.m_pFrames.Items[i]);

    MS.WriteWord(pFrame.m_nDecSize);
    MS.WriteWord(pFrame.m_nWidth);
    MS.WriteWord(pFrame.m_nHeight);

    // test
    {if (m_szName = 'StrobeWaveLightFX0__LA') and (i = 3542) then
    begin
      SaveArrayToTGA(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, '0000.tga', 4, True, False);
      Sleep(0);
    end;}

    if pFrame.m_nSize = 0 then
    begin
      MS.WriteWord(0);
      MS.WriteWord(0);
      Continue;
    end;

    if nLineCounter < LANDSCAPE_LIGHTMAPS_PER_LINE then
    begin
      Inc(nLineWidth, pFrame.m_nWidth);

      if pFrame.m_nHeight > nLineHeight then
        nLineHeight := pFrame.m_nHeight;

      Inc(nLineCounter, 1);

      pFrame.m_nLandscapeX := nLineWidth - pFrame.m_nWidth;
      pFrame.m_nLandscapeY := nFullHeight;

      bFinalLine := True;
    end
    else
    begin
      Inc(nLineWidth, pFrame.m_nWidth);

      if pFrame.m_nHeight > nLineHeight then
        nLineHeight := pFrame.m_nHeight;

      pFrame.m_nLandscapeX := nLineWidth - pFrame.m_nWidth;
      pFrame.m_nLandscapeY := nFullHeight;

      if nLineWidth > nFullWidth then
        nFullWidth := nLineWidth;
      Inc(nFullHeight, nLineHeight);

      nLineCounter := 0;
      nLineWidth := 0;
      nLineHeight := 0;

      bFinalLine := False;
    end;

    MS.WriteWord(pFrame.m_nLandscapeX);
    MS.WriteWord(pFrame.m_nLandscapeY);
  end;

  if bFinalLine then
  begin
    if nLineWidth > nFullWidth then
      nFullWidth := nLineWidth;
    Inc(nFullHeight, nLineHeight);
  end;

  SetLength(Buffer, nFullWidth * nFullHeight * 4);

  for i := 0 to m_nFrames - 1 do
  begin
    pFrame := TLMFrame(pBatch.m_pFrames.Items[i]);

    if pFrame.m_nSize = 0 then
      Continue;

    SimpleBlt32(TDynCardinalArray(Buffer), TDynCardinalArray(pFrame.m_anDecData),
      nFullWidth, pFrame.m_nWidth, pFrame.m_nHeight, pFrame.m_nLandscapeX, pFrame.m_nLandscapeY);

    // test
    {if (m_szName = 'StrobeWaveLightFX0__LA') and (i = 3541) then
    begin
      SaveArrayToTGA(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, '0000SAVE.tga', 4, True, False);
      SaveArrayToTGA(Buffer, nFullWidth, nFullHeight, '0001SAVE.tga', 4, True, False);
      Sleep(0);
    end; }

    // SimpleReverseBlt32 test {
    {SetLength(pFrame.m_anReData, pFrame.m_nDecSize);
    SimpleReverseBlt32(TDynCardinalArray(Buffer), TDynCardinalArray(pFrame.m_anReData),
      nFullWidth, pFrame.m_nWidth, pFrame.m_nHeight, pFrame.m_nLandscapeX, pFrame.m_nLandscapeY);

    if CompareDynArrays(pFrame.m_anDecData, pFrame.m_anReData, pFrame.m_nDecSize) > -1 then
      WriteLn('1111111111111111111111111111'); }
    // SimpleReverseBlt32 test }

    {Buffer[((pFrame.m_nLandscapeY * nFullWidth * 4) + pFrame.m_nLandscapeX * 4) + 0] := $FF;
    Buffer[((pFrame.m_nLandscapeY * nFullWidth * 4) + pFrame.m_nLandscapeX * 4) + 1] := $FF;
    Buffer[((pFrame.m_nLandscapeY * nFullWidth * 4) + pFrame.m_nLandscapeX * 4) + 2] := $FF;}
  end;
end;

procedure TLMAnim.CreatePolyDataLandscape(var Buffer: TDynByteArray; pBatch: TLMBatch; var nFullWidth: Word; var nFullHeight: Word; MS: TMemoryStream);
var i: Cardinal;
    pPolyData: TLMFramePolyData;
    nSize: Cardinal = 0;
    nAdjustedSize: Cardinal = 0;
    nPos: Cardinal = 0;
begin
  for i := 0 to m_nFrames - 1 do
  begin
    pPolyData := TLMFramePolyData(pBatch.m_pFramePolyData.Items[i]);
    Inc(nSize, pPolyData.m_nVertices);
    MS.WriteByte(pPolyData.m_nVertices);
  end;
  nFullWidth := Trunc(sqrt(nSize) + 1);
  nFullHeight := nFullWidth;
  nAdjustedSize := nFullWidth * nFullHeight * 3;

  SetLength(Buffer, nAdjustedSize);
  FillByte(Buffer[0], nAdjustedSize, 0);

  nPos := 0;
  for i := 0 to m_nFrames - 1 do
  begin
    pPolyData := TLMFramePolyData(pBatch.m_pFramePolyData.Items[i]);
    Move(pPolyData.m_anData[0], Buffer[nPos], pPolyData.m_nVertices * 3);
    nPos := nPos + Cardinal(pPolyData.m_nVertices * 3);
  end;
end;

procedure TLMAnim.LoadPolyDataFromStream(pBatch: TLMBatch; MS: TMemoryStream; anBuffer: TDynByteArray);
var i: Cardinal;
    pPolyData: TLMFramePolyData;
    nPos: Cardinal = 0;
    nDataSize: Cardinal;
begin
  for i := 0 to m_nFrames - 1 do
  begin
    pPolyData := TLMFramePolyData.Create;
    pPolyData.m_nVertices := MS.ReadByte;

    nDataSize := pPolyData.m_nVertices * 3;
    SetLength(pPolyData.m_anData, nDataSize);
    Move(anBuffer[nPos], pPolyData.m_anData[0], nDataSize);
    nPos := nPos + nDataSize;

    pBatch.m_pFramePolyData.Add(pPolyData);
  end;
end;

procedure TLMAnim.LoadFramesFromStream(pBatch: TLMBatch; MS: TMemoryStream; anBuffer: TDynByteArray; nFullWidth: Word);
var i: Cardinal;
    pFrame: TLMFrame;
begin
  for i := 0 to m_nFrames - 1 do
  begin
    pFrame := TLMFrame.Create;
    pFrame.m_nDecSize := MS.ReadWord;
    pFrame.m_nWidth := MS.ReadWord;
    pFrame.m_nHeight := MS.ReadWord;
    pFrame.m_nLandscapeX := MS.ReadWord;
    pFrame.m_nLandscapeY := MS.ReadWord;

    //pFrame.m_nDecSize := pFrame.m_nWidth * pFrame.m_nHeight * 4;
    SetLength(pFrame.m_anDecData, pFrame.m_nDecSize);

    if pFrame.m_nDecSize > 0 then
    begin
      SimpleReverseBlt32(TDynCardinalArray(anBuffer), TDynCardinalArray(pFrame.m_anDecData),
        nFullWidth, pFrame.m_nWidth, pFrame.m_nHeight, pFrame.m_nLandscapeX, pFrame.m_nLandscapeY);
    end;

    pBatch.m_pFrames.Add(pFrame);
  end;
end;

procedure TLMAnim.SaveBatchesOnDisk;
var i: Cardinal;
    anBuffer: TDynByteArray;
    nWidth: Word = 0;
    nHeight: Word = 0;
    pBatch: TLMBatch;
    pPolyRef: TLMPolyRef;
    MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  MS.WriteDWord(m_nLMType);
  MS.WriteByte(m_nBatches);
  MS.WriteWord(m_nFrames);

  for i := 0 to m_nFrames - 1 do
  begin
    pPolyRef := TLMPolyRef(m_pPolyRefs.Items[i]);
    MS.WriteWord(pPolyRef.m_nModelIndex);
    MS.WriteWord(pPolyRef.m_nPolyIndex);
  end;

  for i := 0 to m_nBatches - 1 do
  begin
    pBatch := TLMBatch(m_pBatches.Items[i]);
    CreateFramesLandscape(anBuffer{%H-}, pBatch, nWidth, nHeight, MS);

    if nWidth * nHeight > 0 then
      SaveArrayToTGA(anBuffer, nWidth, nHeight, CPData.DumpsDir + CPData.Sep + m_szName + '_' + IntToStr(i) + '.tga', 4, True, False);

    SetLength(anBuffer, 0);

    CreatePolyDataLandscape(anBuffer{%H-}, pBatch, nWidth, nHeight, MS);

    if nWidth * nHeight > 0 then
      SaveArrayToTGA(anBuffer, nWidth, nHeight, CPData.DumpsDir + CPData.Sep + m_szName + '_' + IntToStr(i) + '_polydata.tga', 3, False, False);

    SetLength(anBuffer, 0);
  end;

  MS.SaveToFile(CPData.DumpsDir + CPData.Sep + m_szName + '.index');
  MS.Free;
end;

procedure TLMAnim.LoadBatchesFromDisk;
var MS: TMemoryStream;
    i: Cardinal;
    pPolyRef: TLMPolyRef;
    pBatch: TLMBatch;
    anBuffer: TDynByteArray;
    nFullWidth: Word = 0;
    nFullHeight: Word = 0;
    nChannels: Byte = 0;
    szLMFile, szPDFile: string;
begin
  MS := TMemoryStream.Create;
  MS.LoadFromFile(CPData.DumpsDir + CPData.Sep + m_szName + '.index');

  m_nLMType := MS.ReadDWord;
  m_nBatches := MS.ReadByte;
  m_nFrames := MS.ReadWord;

  for i := 0 to m_nFrames - 1 do
  begin
    pPolyRef := TLMPolyRef.Create;
    pPolyRef.m_nModelIndex := MS.ReadWord;
    pPolyRef.m_nPolyIndex := MS.ReadWord;
    m_pPolyRefs.Add(pPolyRef);
  end;

  for i := 0 to m_nBatches - 1 do
  begin
    pBatch := TLMBatch.Create;

    szLMFile := CPData.DumpsDir + CPData.Sep + m_szName + '_' + IntToStr(i) + '.tga';
    szPDFile := CPData.DumpsDir + CPData.Sep + m_szName + '_' + IntToStr(i) + '_polydata.tga';

    if FileExists(szLMFile) then
      LoadArrayFromTGA(anBuffer{%H-}, nFullWidth, nFullHeight, szLMFile, nChannels, True, False);
    LoadFramesFromStream(pBatch, MS, anBuffer, nFullWidth);

    if FileExists(szPDFile) then
      LoadArrayFromTGA(anBuffer{%H-}, nFullWidth, nFullHeight, szPDFile, nChannels, False, False);
    LoadPolyDataFromStream(pBatch, MS, anBuffer);

    m_pBatches.Add(pBatch);
  end;

  SetLength(anBuffer, 0);
  MS.Free;
end;

constructor TLMAnim.Create;
begin
  m_pPolyRefs := TFPObjectList.Create(True);
  m_pBatches := TFPObjectList.Create(True);
end;

destructor TLMAnim.Destroy;
begin
  inherited Destroy;
  m_pPolyRefs.Free;
end;

procedure TLTWorldPoly.ReadPoly(FS: TMemoryStream);
//var i: Cardinal;
begin
  FS.Read(m_vCenter, SizeOf(LTVector));
  //FS.Read(m_fRadius, 4);

  FS.Read(m_nLightmapWidth, 2);
  FS.Read(m_nLightmapHeight, 2);

  FS.Read(m_nUnknownNum, 2);

  if m_nUnknownNum > 0 then
  begin
    SetLength(m_anUnknownList, m_nUnknownNum * 2);
    FS.Read(m_anUnknownList[0], SizeOf(Word) * m_nUnknownNum * 2);
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
    nTemp: Integer;
    //pVertex: TLTWorldVertex;
begin
  Result := 0;
  dwWorldInfoFlags := 0;
  dwUnknown := 0;
  dwUnknown2 := 0;
  dwUnknown3 := 0;
  nNameLen := 0;
  FS.Read(dwWorldInfoFlags, 4);
  //Assert((dwWorldInfoFlags and $ffff0000) = 0, 'ASSERT FAILED!');
  m_nWorldInfoFlags := dwWorldInfoFlags;
  FS.Read(dwUnknown, 4);
  FS.Read(nNameLen, 2);
  SetLength(m_szWorldName, nNameLen);
  FS.Read(m_szWorldName[1], nNameLen);

  WriteLn('--- Loading BSP: ', m_szWorldName);
  nTemp := Logger.RootLevel;
  Logger.RootLevel := LM_INFO;
  WLogStr('--- Loading BSP: ' + m_szWorldName);
  Logger.RootLevel := nTemp;

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

  FS.Read(m_nNamesLen, 4);
  FS.Read(m_nTextures, 4);

  ReadTextures(FS);
  ReadPolies(FS, False);
  ReadLeafs(FS);
  // fuck this!
  {if m_nLeafs > 0 then
  begin
    Logger.WLog(LM_WARN, 'WorldModel "' + m_szWorldName + '" has leafs > 0');
    Exit(-1);
  end;  }

  ReadPlanes(FS);
  ReadSurfaces(FS);
  ReadPoints(FS);
  ReadPolies(FS, True);
  ReadNodes(FS);
  ReadUserPortals(FS);
  ReadPBlockTable(FS);
  ReadRootNode(FS);

  FS.Read(m_nSections, 4);
  if m_nSections > 0 then
    Logger.WLog(LM_WARN, 'WorldModel has terrain sections > 0');

  w_SetPlaneTypes(m_pNodes, m_pPolies, m_pPlanes, m_nNodes, bUsePlaneTypes);

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
  //FS.Read(m_UnknownStruct{%H-}, SizeOf(TUnknownStruct));
end;

procedure TLTWorldBsp.ReadNodes(FS: TMemoryStream);
var i, j, nPoly, nStatus: Cardinal;
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
    for i := 0 to m_nNodes - 1 do
    begin
      pNode := TLTWorldNode.Create;
      FS.Read(nPoly, 4);
      if nPoly >= m_nPolies then
      begin
        Logger.WLog(LM_ERROR, Format('Node %d has invalid poly index (%d)', [i, nPoly]));
        Exit;
      end;
      pNode.Poly := nPoly;

      FS.Read(nLeaf, 2);
      pNode.Leaf := nLeaf;

      for j := 0 to 1 do
      begin
        FS.Read(nNodeIndex, 4);
        pNode.m_anSides[j] := w_NodeForIndex(m_nNodes, nNodeIndex, nStatus);
        pNode.m_anSidesStatus[j] := nStatus;
      end;

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
    szName: string;
    nTempCardinal: Cardinal = 0;
    nTempWord: Word = 0;
    vTempVector: LTVector;
begin
  if m_nUserPortals > 0 then
  begin
    for i := 0 to m_nUserPortals - 1 do
    begin
      FS.Read(nNameLen, 2);
      SetLength(szName, nNameLen);
      FS.Read(szName[1], nNameLen);
      FS.Read(nTempCardinal, 4);
      FS.Read(nTempCardinal, 4);
      FS.Read(nTempWord, 2);
      FS.Read(vTempVector{%H-}, SizeOf(LTVector));
      FS.Read(vTempVector, SizeOf(LTVector));
    end;
  end;
end;

procedure TLTWorldBsp.ReadPBlockTable(FS: TMemoryStream);
var nTempCardinal1, nTempCardinal2, nTempCardinal3, i: Cardinal;
    nCounterCardinal: Cardinal = 0;
    vTempVector: LTVector;
    nCounterWord: Word = 0;
    nTempWord: Word = 0;
    anTempBuffer: array of Byte;
begin
  nTempCardinal1 := 0;
  nTempCardinal2 := 0;
  nTempCardinal3 := 0;
  FS.Read(nTempCardinal1, 4);
  FS.Read(nTempCardinal2, 4);
  FS.Read(nTempCardinal3, 4);

  nCounterCardinal := nTempCardinal1 * nTempCardinal2 * nTempCardinal3;

  FS.Read(vTempVector{%H-}, SizeOf(LTVector));
  FS.Read(vTempVector, SizeOf(LTVector));

  if nCounterCardinal > 0 then
  begin
    for i := 0 to nCounterCardinal - 1 do
    begin
      FS.Read(nCounterWord, 2);
      FS.Read(nTempWord, 2);

      if nCounterWord > 0 then
      begin
        SetLength(anTempBuffer, 6 * nCounterWord);
        FS.Read(anTempBuffer[0], 6 * nCounterWord);
        SetLength(anTempBuffer, 0);
      end;
    end;
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
      Logger.WLog(LM_WARN, 'Texture #' + IntToStr(i) + ' has zero length!');
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
    nNumLeafLists: Word = 0;
    nTempWord: Word = 0;
    anTempArray: array of Byte;
    nPoliesCount: Cardinal = 0;
    nUnknownCardinal: Cardinal = 0;
begin
  if m_nLeafs > 0 then
  begin
    for i := 0 to m_nLeafs - 1 do
    begin
      FS.Read(nNumLeafLists, 2);
      if nNumLeafLists = $FFFF then
      begin
        FS.Read(nTempWord, 2);
      end
      else
      begin
        if nNumLeafLists > 0 then
        begin
          for j := 0 to nNumLeafLists - 1 do
          begin
            FS.Read(nTempWord, 2);
            FS.Read(nTempWord, 2);
            SetLength(anTempArray, nTempWord);
            FS.Read(anTempArray[0], nTempWord);
            SetLength(anTempArray, 0);
          end;
        end;
      end;
      FS.Read(nPoliesCount, 4);
      if nPoliesCount > 0 then
      begin
        for j := 0 to nPoliesCount - 1 do
        begin
          FS.Read(nTempWord, 2);
          FS.Read(nTempWord, 2);
        end;
      end;
      FS.Read(nUnknownCardinal, 4);
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

