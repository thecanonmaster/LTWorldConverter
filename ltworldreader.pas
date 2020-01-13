unit ltworldreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ltworldtypes, ltworldobject, contnrs, ltworldtree,
  ltworlddata, globals, MyLogger;

type

  { TLTWorldReader }

  TLTWorldReader = class(TObject)
  private
    // data
    WorldHeader: TWorldHeader;
    WorldProperties: string;
    WorldExtents: TWorldExtents;
    WorldObjectList: TWorldObjectList;
    WorldTree: TLTWorldTree;
    WorldModelList: TWorldModelList;

    // other
    m_szFilename: string;
    //m_pFileStream: TFileStream;
    m_pMemoryStream: TMemoryStream;
    m_bDumpNodes: Boolean;

    // routines
    procedure ReadHeader;
    procedure ReadPropertiesAndExtents;
    procedure ReadWorldTree;
    procedure ReadObjects;
    procedure ReadWorldModels;
    //procedure ReadTemp;

  protected
  public
    procedure ReadWorld;

    property Header: TWorldHeader read WorldHeader write WorldHeader;
    property Properties: string read WorldProperties write WorldProperties;
    property Extents: TWorldExtents read WorldExtents write WorldExtents;
    property ObjectList: TWorldObjectList read WorldObjectList write WorldObjectList;
    property Tree: TLTWorldTree read WorldTree write WorldTree;
    property ModelList: TWorldModelList read WorldModelList write WorldModelList;

    constructor Create(szFileName: string; bDumpNodes: Boolean); virtual;
    destructor Destroy; override;
  end;

implementation

constructor TLTWorldReader.Create(szFileName: string; bDumpNodes: Boolean);
begin
  m_bDumpNodes := bDumpNodes;
  m_szFilename := szFileName;
  WLogStr('LTWorldReader created: ' + m_szFilename);
  //m_pFileStream := TFileStream.Create(m_szFilename, fmOpenRead);
  m_pMemoryStream := TMemoryStream.Create;
  m_pMemoryStream.LoadFromFile(m_szFilename);
  //m_pFileStream.Free;
  WorldObjectList.pObjectList := TFPObjectList.Create(True);
  WorldModelList.pModelList := TFPObjectList.Create(True);
end;

destructor TLTWorldReader.Destroy;
begin
  WorldTree.Free;
  m_pMemoryStream.Free;
  WorldObjectList.pObjectList.Free;
  WorldModelList.pModelList.Free;
  WLogStr('LTWorldReader destroyed');
  inherited;
end;

procedure TLTWorldReader.ReadPropertiesAndExtents;
var
  nLen: integer;
begin
  nLen := 0;
  m_pMemoryStream.Read(nLen, 4);
  SetLength(WorldProperties, nLen);
  if nLen > 0 then m_pMemoryStream.Read(WorldProperties[1], nLen);
  m_pMemoryStream.Read(WorldExtents.fUnknown, 4);
  m_pMemoryStream.Read(WorldExtents.vExtentsMin, sizeof(LTVector));
  m_pMemoryStream.Read(WorldExtents.vExtentsMax, sizeof(LTVector));
  //m_pMemoryStream.Read(WorldExtents.vOffset, sizeof(LTVector));
  // now log it!
  WLogStr('---- WorldProperties and WorldExtents:');
  WLogStr('| Properties = ' + WorldProperties);
  WLogReal('UnknownReal', WorldExtents.fUnknown);
  WLogVec('ExtentsMin', @WorldExtents.vExtentsMin);
  WLogVec('ExtentsMax', @WorldExtents.vExtentsMax);
  WLogVec('Offset', @WorldExtents.vOffset);
  WLogStr('-----------------------------------');
end;

procedure TLTWorldReader.ReadHeader;
begin
  m_pMemoryStream.Read(WorldHeader.nVersion, 4);
  m_pMemoryStream.Read(WorldHeader.dwObjectDataPos, 4);
  m_pMemoryStream.Read(WorldHeader.dwRenderDataPos, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy1, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy2, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy3, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy4, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy5, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy6, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy7, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy8, 4);
  // now log it!
  WLogStr('---- WorldHeader:');
  WLogInt('Version', WorldHeader.nVersion);
  WLogAddr('ObjectDataPos', WorldHeader.dwObjectDataPos);
  WLogAddr('RenderDataPos', WorldHeader.dwRenderDataPos);
  WLogAddr('Dummy1', WorldHeader.dwDummy1);
  WLogAddr('Dummy2', WorldHeader.dwDummy2);
  WLogAddr('Dummy3', WorldHeader.dwDummy3);
  WLogAddr('Dummy4', WorldHeader.dwDummy4);
  WLogAddr('Dummy5', WorldHeader.dwDummy5);
  WLogAddr('Dummy6', WorldHeader.dwDummy6);
  WLogAddr('Dummy7', WorldHeader.dwDummy7);
  WLogAddr('Dummy8', WorldHeader.dwDummy8);
  WLogStr('-----------------------------------');
end;

procedure TLTWorldReader.ReadObjects;
var
  i, j, file_pos: Cardinal;
  WorldObject: TLTWorldObject;
  nWorldModelsNum: Cardinal;
begin
  nWorldModelsNum := 0;
  m_pMemoryStream.Read(WorldObjectList.nNumObjects, 4);
  WLogStr('---- WorldObjects:');
  WLogInt('NumObjects', WorldObjectList.nNumObjects);
  if WorldObjectList.nNumObjects > 0 then
  for i := 0 to WorldObjectList.nNumObjects - 1 do
  begin
    WorldObject := TLTWorldObject.Create;
    file_pos := m_pMemoryStream.Position;
    WorldObject.ReadObject(m_pMemoryStream);
    WLogStr('| ' + WorldObject.GetObjectName + '(' + WorldObject.TypeString + ')' + '[' + IntToHex(file_pos, 8) + ']');
    // debug count
    if WorldObject.TypeString = 'TranslucentWorldModel' then Inc(nWorldModelsNum, 1);
    for j := 0 to WorldObject.NumProperties - 1 do
    begin
      WLogStr('| | ' + TLTWorldObjectProperty(WorldObject.PropertyList.Items[j]).PropName +
      '[' + IntToStr(TLTWorldObjectProperty(WorldObject.PropertyList.Items[j]).PropCode) + '] = ' +
      TLTWorldObjectProperty(WorldObject.PropertyList.Items[j]).DataGeneric);
    end;
    WLogStr('| ----------------------------------');
    WorldObjectList.pObjectList.Add(WorldObject);
  end;
  //WLogInt('WorldModels count', nWorldModelsNum);
  WLogStr('-----------------------------------');
end;

procedure TLTWorldReader.ReadWorldTree;
var i: Cardinal;
begin
  WorldTree := TLTWorldTree.Create;
  WorldTree.ReadWorldTree(m_pMemoryStream);
  WLogStr('---- WorldTree:');
  WLogInt('NumNodes', WorldTree.NumNodes);
  WLogStr('| RootNode ');
  WLogVec('| BoxMin', @WorldTree.RootNode.BoxMin);
  WLogVec('| BoxMax', @WorldTree.RootNode.BoxMax);
  WLogReal('| CenterX', WorldTree.RootNode.CenterX);
  WLogReal('| CenterZ', WorldTree.RootNode.CenterZ);
  WLogReal('| SmallestDim', WorldTree.RootNode.SmallestDim);
  WLogStr('| ----------------------------------');

  if WorldTree.NumNodes > 1 then
  for i := 0 to WorldTree.NumNodes - 2 do
  begin
    WLogStr('| Node #' + IntToStr(i));
    WLogVec('| BoxMin', @TLTWorldTreeNode(WorldTree.Nodes.Items[i]).BoxMin);
    WLogVec('| BoxMax', @TLTWorldTreeNode(WorldTree.Nodes.Items[i]).BoxMax);
    WLogReal('| CenterX', TLTWorldTreeNode(WorldTree.Nodes.Items[i]).CenterX);
    WLogReal('| CenterZ', TLTWorldTreeNode(WorldTree.Nodes.Items[i]).CenterZ);
    WLogReal('| SmallestDim', TLTWorldTreeNode(WorldTree.Nodes.Items[i]).SmallestDim);
    WLogStr('| ----------------------------------');
  end;

  WLogStr('-----------------------------------');
end;

procedure TLTWorldReader.ReadWorldModels;
var i, j, k, nDummy: Cardinal;
    pWorldData: TLTWorldData;
    pWorldBSP: TLTWorldBsp;
    pWorldPoly: TLTWorldPoly;
    pWorldNode: TLTWorldNode;
    anDummy: array[0..$1F] of Byte;
    szDump: string;
    LoadBSPResult: Integer;
begin
  WLogStr('---- WorldData');
  m_pMemoryStream.Read(WorldModelList.nNumModels, 4);
  WLogInt('NumWorldModels', WorldModelList.nNumModels);
  for i := 0 to WorldModelList.nNumModels - 1 do
  begin
    nDummy := 0;
    WLogAddr('WorldData starts', m_pMemoryStream.Position);
    m_pMemoryStream.Read(nDummy, 4);
    m_pMemoryStream.Read(anDummy{%H-}, $20);
    pWorldData := TLTWorldData.Create;
    pWorldData.NextPos := nDummy;
    WorldModelList.pModelList.Add(pWorldData);

    pWorldBSP := TLTWorldBsp.Create;
    LoadBSPResult := pWorldBSP.Load(m_pMemoryStream, True);
    pWorldData.OriginalBSP := pWorldBSP;

    WLogStr('| ' + pWorldBSP.WorldName);
    WLogInt('| Index', i);
    WLogInt('| DataFlags', pWorldData.Flags);
    WLogInt('| InfoFlags', pWorldBSP.WorldInfoFlags);
    WLogInt('| PointsNum', pWorldBSP.Points);
    WLogInt('| PlainsNum', pWorldBSP.Planes);
    WLogInt('| SurfacesNum', pWorldBSP.Surfaces);
    WLogInt('| UserPortalsNum', pWorldBSP.UserPortals);
    WLogInt('| PoliesNum', pWorldBSP.Polies);
    WLogInt('| LeafsNum', pWorldBSP.Leafs);
    WLogInt('| VertsNum', pWorldBSP.Verts);
    WLogInt('| TotalVisListSize', pWorldBSP.TotalVisListSize);
    WLogInt('| LeafListsNum', pWorldBSP.LeafLists);
    WLogInt('| NodesNum', pWorldBSP.Nodes);
    WLogInt('| TexturesNum', pWorldBSP.Textures);

    WLogVec('| MinBox', @pWorldBSP.MinBox);
    WLogVec('| MaxBox', @pWorldBSP.MaxBox);
    WLogVec('| WorldTranslation', @pWorldBSP.WorldTranslation);

    for j := 0 to Length(pWorldBSP.TextureNames) - 1 do
    begin
      WLogStr('| | Texture' + IntToStr(j) + ' = ' + pWorldBSP.TextureNames[j]);
    end;

    szDump := '';

    if pWorldBSP.Polies > 0 then
    for j := 0 to pWorldBSP.Polies - 1 do
    begin
      szDump := szDump + ' [' + IntToStr(TLTWorldPoly(pWorldBSP.PoliesList.Items[j]).GetIndex) +
      ', ' + IntToStr(TLTWorldPoly(pWorldBSP.PoliesList.Items[j]).GetNumVertices) + ']';
    end;
    WLogStr('| | PoliesDump =' + szDump);

    // fuck this!
    if LoadBSPResult = -1 then
    begin
      Logger.WLog(LM_WARN, Format('WorldModel %s is not loaded!', [pWorldBSP.WorldName]));
      m_pMemoryStream.Position := pWorldData.NextPos;
      Continue;
    end;

    // save to file
    if m_bDumpNodes then
      pWorldBSP.SaveNodesDump(CPData.Dir + CPData.Sep + 'dumps' + CPData.Sep + pWorldBSP.WorldName + '.ltnd');

    if pWorldBSP.Planes > 0 then
    for j := 0 to pWorldBSP.Planes - 1 do
    begin
      WLogStr('| | Plane #' + IntToStr(j));
      WLogVec('| | Normal', @TLTWorldPlane(pWorldBSP.PlanesList.Items[j]).m_vNormal);
      WLogReal('| | Dist', TLTWorldPlane(pWorldBSP.PlanesList.Items[j]).m_fDist);
      WLogStr('| | ----------------------------------');
    end;

    if pWorldBSP.Surfaces > 0 then
    for j := 0 to pWorldBSP.Surfaces - 1 do
    begin
      WLogStr('| | Surface #' + IntToStr(j));
      WLogVec('| | UV[0]', @TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_fUV1);
      WLogVec('| | UV[1]', @TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_fUV2);
      WLogVec('| | UV[2]', @TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_fUV3);
      WLogInt('| | TextureIndex', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nTexture);
      WLogInt('| | SurfaceFlags', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nFlags);
      WLogStr('| | | SurfaceFlagsBin = ' + binStr(TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nFlags, 32));
      WLogInt('| | Unknown1', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nUnknown1);
      WLogInt('| | UseEffect', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nUseEffect);
      WLogStr('| | | Effect = ' + TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_szEffect);
      WLogStr('| | | EffectParam = ' + TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_szEffectParam);
      WLogInt('| | TextureFlags', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nTextureFlags);
      WLogStr('| | ----------------------------------');
    end;

    if pWorldBSP.Points > 0 then
    begin
      WLogStr('| | Points');
      for j := 0 to pWorldBSP.Points - 1 do
      begin
        WLogVec('| | Vertex[' + IntToStr(j) + ']', @TLTWorldVertex(pWorldBSP.PointsList.Items[j]).m_vData);
      end;
      WLogStr('| | ----------------------------------');
    end;

    if pWorldBSP.Polies > 0 then
    for j := 0 to pWorldBSP.Polies - 1 do
    begin
      pWorldPoly := TLTWorldPoly(pWorldBSP.PoliesList.Items[j]);

      WLogStr('| | Poly #' + IntToStr(j));

      if pWorldPoly.HiVerts > 0 then
         Logger.WLog(LM_INFO, '| | This poly has additional vertices [' + IntToStr(pWorldPoly.HiVerts) + ']');

      WLogVec('| | Center', @pWorldPoly.Center);
      //WLogReal('| | Radius', pWorldPoly.Radius);
      WLogInt('| | UnknownFlags', pWorldPoly.UnknownFlags);
      WLogInt('| | UnknownNum', pWorldPoly.UnknownNum);

      szDump := '';

      if pWorldPoly.UnknownNum > 0 then
      for k := 0 to pWorldPoly.UnknownNum - 1 do
      begin
        szDump := szDump + ' [' + IntToStr(pWorldPoly.UnknownList[k]) + ']';
      end;
      WLogStr('| | | UnknownDump =' + szDump);

      WLogInt('| | SurfaceIndex', pWorldPoly.Surface);
      WLogInt('| | PlaneIndex', pWorldPoly.Plane);

      WLogVec('| | UV[0]', @pWorldPoly.UVData1);
      WLogVec('| | UV[1]', @pWorldPoly.UVData2);
      WLogVec('| | UV[2]', @pWorldPoly.UVData3);

      szDump := '';
      for k := 0 to pWorldPoly.GetNumVertices - 1 do
      begin
        szDump:= szDump + ' [' + IntToStr(pWorldPoly.DiskVerts[k].nVerts) + ', ' + IntToStr(pWorldPoly.DiskVerts[k].nDummy1) +
                                                         ', ' + IntToStr(pWorldPoly.DiskVerts[k].nDummy2) +
                                                         ', ' + IntToStr(pWorldPoly.DiskVerts[k].nDummy3) + ']';
      end;
      WLogStr('| | | VertexNumDump =' + szDump);

      WLogStr('| | ----------------------------------');
    end;

    if pWorldBSP.Nodes > 0 then
    for j := 0 to pWorldBSP.Nodes - 1 do
    begin
      pWorldNode := TLTWorldNode(pWorldBSP.NodesList.Items[j]);
      WLogStr('| | Node #' + IntToStr(j));
      WLogInt('| | Flags', pWorldNode.Flags);
      WLogInt('| | PlaneType', pWorldNode.PlaneType);
      WLogInt('| | PolyIndex', pWorldNode.Poly);
      WLogInt('| | LeafIndex', pWorldNode.Leaf);
      WLogInt('| | Sides[0]', pWorldNode.Sides[0]);
      WLogInt('| | Sides[1]', pWorldNode.Sides[1]);
      WLogInt('| | SidesStatus[0]', pWorldNode.SidesStatus[0]);
      WLogInt('| | SidesStatus[1]', pWorldNode.SidesStatus[1]);
      WLogStr('| | ----------------------------------');
    end;

    WLogStr('| | UnknownStruct after Nodes');
    WLogInt('| | Cardinal1', pWorldBSP.UnknownStruct.n1);
    WLogInt('| | Cardinal2', pWorldBSP.UnknownStruct.n2);
    WLogInt('| | Cardinal3', pWorldBSP.UnknownStruct.n3);
    WLogVec('| | Vector1', @pWorldBSP.UnknownStruct.v1);
    WLogVec('| | Vector2', @pWorldBSP.UnknownStruct.v2);

    WLogStr('| ----------------------------------');
    m_pMemoryStream.Position := pWorldData.NextPos;
  end;

  WLogStr('-----------------------------------');
  WLogStr('ReadPos: ' + IntToHex(m_pMemoryStream.Position, 8));
end;

procedure TLTWorldReader.ReadWorld;
begin
  ReadHeader;
  ReadPropertiesAndExtents;
  ReadWorldTree;
  ReadWorldModels;
  // test
  //m_pMemoryStream.Position := WorldHeader.dwRenderDataPos;

  m_pMemoryStream.Position := WorldHeader.dwObjectDataPos;
  ReadObjects;
  //ReadTemp;
end;

end.
