unit ltworldtree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ltworldtypes, math, contnrs;

const
  MAX_WTNODE_CHILDREN = 4;

type
  TLTWorldTreeNode = class(TObject)
  private
    m_vBBoxMin: LTVector;
    m_vBBoxMax: LTVector;
    m_fCenterX: LTFloat;
    m_fCenterZ: LTFloat;
    m_fSmallestDim: LTFloat;
    m_pParent: TLTWorldTreeNode;
    m_nChildren: Cardinal;
    m_pNodeList: TFPObjectList;
  protected
  public
    property BoxMin: LTVector read m_vBBoxMin write m_vBBoxMin;
    property BoxMax: LTVector read m_vBBoxMax write m_vBBoxMax;
    property CenterX: LTFloat read m_fCenterX write m_fCenterX;
    property CenterZ: LTFloat read m_fCenterZ write m_fCenterZ;
    property SmallestDim: LTFloat read m_fSmallestDim write m_fSmallestDim;
    function GetChild(nX, nZ: Cardinal): TLTWorldTreeNode;
    function GetChild(nNode: Cardinal): TLTWorldTreeNode;
    procedure SetBBox(pMin: PLTVector; pMax: PLTVector);
    procedure LoadLayout(FS: TMemoryStream; var nCurByte: Byte; var nCurBit: Byte; pNodeList: TFPObjectList; var nCurOffset: Cardinal);
    procedure Subdivide({%H-}pNodeList: TFPObjectList; var nCurOffset: Cardinal);
    constructor Create(pNodeList: TFPObjectList); virtual;
    destructor Destroy; override;
  end;

  TLTWorldTree = class(TObject)
  private
    m_nNumNodes: Cardinal;
    m_pRootNode: TLTWorldTreeNode;
    m_pNodes: TFPObjectList;
  protected
  public
    property NumNodes: Cardinal read m_nNumNodes write m_nNumNodes;
    property RootNode: TLTWorldTreeNode read m_pRootNode write m_pRootNode;
    property Nodes: TFPObjectList read m_pNodes write m_pNodes;
    procedure ReadWorldTree(FS: TMemoryStream);
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

procedure TLTWorldTree.ReadWorldTree(FS: TMemoryStream);
var nDummyTerrainDepth, nCurOffset, i: Cardinal;
    vBoxMin, vBoxMax: LTVector;
    nCurByte, nCurBit: Byte;
    pNewNode: TLTWorldTreeNode;
begin
  nDummyTerrainDepth := 0;
  vBoxMin := LTVectorInit(0, 0, 0);
  vBoxMax := LTVectorInit(0, 0, 0);

  //FS.Position := FS.Position - 8;

  FS.Read(vBoxMin, sizeof(LTVector));
  // omg!
  FS.Read(vBoxMax, sizeof(LTVector));
  FS.Read(m_nNumNodes, 4);
  FS.Read(nDummyTerrainDepth, 4);

  if m_nNumNodes > 1 then
  for i := 0 to m_nNumNodes - 2 do
  begin
    pNewNode := TLTWorldTreeNode.Create(m_pNodes);
    m_pNodes.Add(pNewNode);
  end;

  nCurByte := 0;
  nCurBit := 8;
  m_pRootNode.SetBBox(@vBoxMin, @vBoxMax);

  nCurOffset := 0;
  m_pRootNode.LoadLayout(FS, nCurByte, nCurBit, m_pNodes, nCurOffset);
end;

constructor TLTWorldTree.Create;
begin
  m_pNodes := TFPObjectList.Create(True);
  m_pRootNode := TLTWorldTreeNode.Create(m_pNodes);
end;

destructor TLTWorldTree.Destroy;
begin
  inherited;
  m_pNodes.Free;
  m_pRootNode.Free;
end;

procedure TLTWorldTreeNode.SetBBox(pMin: PLTVector; pMax: PLTVector);
begin
  m_vBBoxMin := pMin^;
  m_vBBoxMax := pMax^;

  m_fCenterX := (pMax^.x + pMin^.x) * 0.5;
  m_fCenterZ := (pMax^.z + pMin^.z) * 0.5;

  m_fSmallestDim := Min(pMax^.x - pMin^.x, pMax^.z - pMin^.z);
end;

function TLTWorldTreeNode.GetChild(nX, nZ: Cardinal): TLTWorldTreeNode;
begin
  Result := TLTWorldTreeNode(m_pNodeList.Items[m_nChildren + (nX * 2 + nZ)]);
end;

function TLTWorldTreeNode.GetChild(nNode: Cardinal): TLTWorldTreeNode;
begin
  Result := TLTWorldTreeNode(m_pNodeList.Items[m_nChildren + nNode]);
end;

procedure TLTWorldTreeNode.LoadLayout(FS: TMemoryStream; var nCurByte: Byte; var nCurBit: Byte; pNodeList: TFPObjectList; var nCurOffset: Cardinal);
var bSubdivide: Boolean;
    i: Cardinal;
begin
  if nCurBit = 8 then
  begin
    FS.Read(nCurByte, 1);
    nCurBit := 0;
  end;
  //bSubdivide := not(not((nCurByte > 0) and ((1 shl nCurBit) > 0)));

  bSubdivide := (nCurByte and (1 shl nCurBit)) > 0;
  Inc(nCurBit, 1);

  if bSubdivide then
  begin
    Subdivide(pNodeList, nCurOffset);
    for i := 0 to MAX_WTNODE_CHILDREN - 1 do
    begin
      GetChild(i).LoadLayout(FS, nCurByte, nCurBit, pNodeList, nCurOffset);
    end;
  end;
end;

procedure TLTWorldTreeNode.Subdivide(pNodeList: TFPObjectList; var nCurOffset: Cardinal);
var i: Cardinal;
    vMin, vMax: LTVector;
begin
  m_nChildren := nCurOffset;
  nCurOffset := nCurOffset + MAX_WTNODE_CHILDREN;

  for i := 0 to MAX_WTNODE_CHILDREN - 1 do
  begin
    GetChild(i).m_pParent := Self;
  end;

  // -x -z
  vMin := LTVectorInit(m_vBBoxMin.x, m_vBBoxMin.y, m_vBBoxMin.z);
  vMax := LTVectorInit(m_fCenterX, m_vBBoxMax.y, m_fCenterZ);
  GetChild(0, 0).SetBBox(@vMin, @vMax);

  // -x +z
  vMin := LTVectorInit(m_vBBoxMin.x, m_vBBoxMin.y, m_fCenterZ);
  vMax := LTVectorInit(m_fCenterX, m_vBBoxMax.y, m_vBBoxMax.z);
  GetChild(0, 1).SetBBox(@vMin, @vMax);

  // +x -z
  vMin := LTVectorInit(m_fCenterX, m_vBBoxMin.y, m_vBBoxMin.z);
  vMax := LTVectorInit(m_vBBoxMax.x, m_vBBoxMax.y, m_fCenterZ);
  GetChild(1, 0).SetBBox(@vMin, @vMax);

  // +x +z
  vMin := LTVectorInit(m_fCenterX, m_vBBoxMin.y, m_fCenterZ);
  vMax := LTVectorInit(m_vBBoxMax.x, m_vBBoxMax.y, m_vBBoxMax.z);
  GetChild(1, 1).SetBBox(@vMin, @vMax);
end;

constructor TLTWorldTreeNode.Create(pNodeList: TFPObjectList);
begin
  m_pNodeList := pNodeList;
end;

destructor TLTWorldTreeNode.Destroy;
begin
  inherited;
end;

end.

