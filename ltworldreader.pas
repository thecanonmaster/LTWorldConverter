unit ltworldreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ltworldtypes, ltworldobject, contnrs, ltworldtree,
  ltworlddata, globals, MyLogger, lightmapcompress;

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
    WorldLMAnimList: TWorldLMAnimList;
    WorldExtentsSH: TWorldExtentsSH;

    // other
    m_szFilename: string;
    //m_pFileStream: TFileStream;
    m_pMemoryStream: TMemoryStream;

    // routines
    procedure ReadHeader;
    procedure ReadPropertiesAndExtents;
    procedure ReadWorldTree;
    procedure ReadObjects;
    procedure ReadWorldModels;
    procedure ReadRenderData(bSave: Boolean);

    procedure WriteRenderData;

    procedure AssignWorldPolyToLMPolyRef(pPolyRef: TLMPolyRef);
    procedure AssignWidthAndHeightToLMFrame(pAnim: TLMAnim; nIndex: Cardinal; pFrame: TLMFrame);

  protected
  public
    procedure ReadWorld;

    procedure RemoveWorldModel(szName: string);
    procedure RemoveWorldObject(szName: string);
    function FindWorldObject(szName: string): TLTWorldObject;
    function GetObjectDWordColor(szLightAnimName: string): Cardinal;
    procedure MoveWorldModel(szName: string; nToIndex: Integer);

    property Header: TWorldHeader read WorldHeader write WorldHeader;
    property Properties: string read WorldProperties write WorldProperties;
    property Extents: TWorldExtents read WorldExtents write WorldExtents;
    property ObjectList: TWorldObjectList read WorldObjectList write WorldObjectList;
    property Tree: TLTWorldTree read WorldTree write WorldTree;
    property ModelList: TWorldModelList read WorldModelList write WorldModelList;
    property LMAnimList: TWorldLMAnimList read WorldLMAnimList write WorldLMAnimList;

    constructor Create(szFileName: string); virtual;
    destructor Destroy; override;
  end;

implementation

constructor TLTWorldReader.Create(szFileName: string);
begin
  m_szFilename := szFileName;
  WLogStr('LTWorldReader created: ' + m_szFilename);
  //m_pFileStream := TFileStream.Create(m_szFilename, fmOpenRead);
  m_pMemoryStream := TMemoryStream.Create;
  m_pMemoryStream.LoadFromFile(m_szFilename);
  //m_pFileStream.Free;
  WorldObjectList.pObjectList := TFPObjectList.Create(True);
  WorldModelList.pModelList := TFPObjectList.Create(True);
  WorldLMAnimList.pLMAnimList := TFPObjectList.Create(True);
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
  {m_pMemoryStream.Read(WorldExtents.fLMGridSize, 4);
  m_pMemoryStream.Read(WorldExtents.vExtentsMin, sizeof(LTVector));
  m_pMemoryStream.Read(WorldExtents.vExtentsMax, sizeof(LTVector));
  m_pMemoryStream.Read(WorldExtents.vOffset, sizeof(LTVector)); }
  // now log it!
  WLogStr('---- WorldProperties and WorldExtents:');
  WLogStr('| Properties = ' + WorldProperties);
  {WLogReal('LMGridSize', WorldExtents.fLMGridSize);
  WLogVec('ExtentsMin', @WorldExtents.vExtentsMin);
  WLogVec('ExtentsMax', @WorldExtents.vExtentsMax);
  WLogVec('Offset', @WorldExtents.vOffset);  }
  WLogStr('-----------------------------------');
end;

procedure TLTWorldReader.ReadHeader;
begin
  m_pMemoryStream.Read(WorldHeader.nVersion, 4);
  m_pMemoryStream.Read(WorldHeader.dwObjectDataPos, 4);
  m_pMemoryStream.Read(WorldHeader.dwBspDataPos, 4);
  {m_pMemoryStream.Read(WorldHeader.dwRenderDataPos, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy1, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy2, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy3, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy4, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy5, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy6, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy7, 4);
  m_pMemoryStream.Read(WorldHeader.dwDummy8, 4); }
  // now log it!
  WLogStr('---- WorldHeader:');
  WLogInt('Version', WorldHeader.nVersion);
  WLogAddr('ObjectDataPos', WorldHeader.dwObjectDataPos);
  WLogAddr('BspDataPos', WorldHeader.dwBspDataPos);
  {WLogAddr('RenderDataPos', WorldHeader.dwRenderDataPos);
  WLogAddr('Dummy1', WorldHeader.dwDummy1);
  WLogAddr('Dummy2', WorldHeader.dwDummy2);
  WLogAddr('Dummy3', WorldHeader.dwDummy3);
  WLogAddr('Dummy4', WorldHeader.dwDummy4);
  WLogAddr('Dummy5', WorldHeader.dwDummy5);
  WLogAddr('Dummy6', WorldHeader.dwDummy6);
  WLogAddr('Dummy7', WorldHeader.dwDummy7);
  WLogAddr('Dummy8', WorldHeader.dwDummy8); }
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
    //if WorldObject.TypeString = 'TranslucentWorldModel' then Inc(nWorldModelsNum, 1);
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
var i, j, k, l, nDummy: Cardinal;
    pWorldData: TLTWorldData;
    pWorldBSP: TLTWorldBsp;
    pWorldPoly: TLTWorldPoly;
    pWorldNode: TLTWorldNode;
    pLeaf: TLTLeaf;
    pLeafList: TLTLeafList;
    pUserPortal: TLTUserPortal;
    pPBlockTable: TLTPBlockTable;
    pPBlockRecord: TLTPBlockRecord;
    anDummy: array[0..$1F] of Byte;
    szDump: string;
    LoadBSPResult: Integer;
begin
  WLogStr('---- WorldData');
  //m_pMemoryStream.Read(WorldModelList.nNumModels, 4);
  //WorldModelList.nNumModels := 1;
  //WLogInt('NumWorldModels', WorldModelList.nNumModels);

  //for i := 0 to WorldModelList.nNumModels - 1 do
  i := 0;
  while True do
  begin
    if i = 1 then
    begin
      m_pMemoryStream.Read(WorldModelList.nNumModels, 4);
      WorldModelList.nNumModels := WorldModelList.nNumModels + 1;
    end;

    if (i > 0) and (i = WorldModelList.nNumModels) then
      Break;

    nDummy := 0;
    WLogAddr('WorldModel starts', m_pMemoryStream.Position);
    m_pMemoryStream.Read(nDummy, 4);

    //if i = 0 then
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
      WLogStrWarn(Format('WorldModel %s is not loaded!', [pWorldBSP.WorldName]));
      m_pMemoryStream.Position := pWorldData.NextPos;
      Continue;
    end;

    // save to file
    if g_bDumpNodes then
      pWorldBSP.SaveNodesDump(CPData.DumpsDir + CPData.Sep + pWorldBSP.WorldName + '.ltnd');

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
      WLogVec('| | UnknownVector1', @TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_vUnknown1);
      WLogVec('| | UnknownVector2', @TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_vUnknown2);
      WLogVec('| | UnknownVector3', @TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_vUnknown3);
      WLogInt('| | TextureIndex', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nTexture);
      WLogInt('| | Unknown0', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nUnknown0);
      WLogInt('| | SurfaceFlags', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nFlags);
      //WLogStr('| | | SurfaceFlagsBin = ' + binStr(TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nFlags, 32));
      WLogInt('| | Unknown1', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nUnknown1);
      WLogInt('| | Unknown2', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nUnknown2);
      WLogInt('| | Unknown3', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nUnknown3);
      WLogInt('| | Unknown4', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nUnknown4);
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
         WLogStr('| | This poly has additional vertices [' + IntToStr(pWorldPoly.HiVerts) + ']');

      //WLogVec('| | Center', @pWorldPoly.Center);
      //WLogReal('| | Radius', pWorldPoly.Radius);
      WLogInt('| | LightmapWidth?', pWorldPoly.LightmapWidth);
      WLogInt('| | LightmapHeight?', pWorldPoly.LightmapHeight);
      //WLogInt('| | UnknownNum', pWorldPoly.UnknownNum);

      szDump := '';

      {if pWorldPoly.UnknownNum > 0 then
      for k := 0 to pWorldPoly.UnknownNum - 1 do
      begin
        szDump := szDump + ' [' + IntToStr(pWorldPoly.UnknownList[k]) + ', ' + IntToStr(pWorldPoly.UnknownList[k+1]) + ']';
      end;
      WLogStr('| | | UnknownDump =' + szDump);}

      WLogInt('| | UnknownCardinal1', pWorldPoly.UnknownCardinal1);
      WLogInt('| | UnknownCardinal2', pWorldPoly.UnknownCardinal2);
      WLogInt('| | SurfaceIndex', pWorldPoly.Surface);
      //WLogInt('| | PlaneIndex', pWorldPoly.Plane);

      //WLogVec('| | UV[0]', @pWorldPoly.UVData1);
      //WLogVec('| | UV[1]', @pWorldPoly.UVData2);
      //WLogVec('| | UV[2]', @pWorldPoly.UVData3);

      WLogInt('| | LoVerts', pWorldPoly.LoVerts);
      WLogInt('| | HiVerts', pWorldPoly.HiVerts);

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
    for j := 0 to pWorldBSP.Nodes do
    begin
      pWorldNode := TLTWorldNode(pWorldBSP.NodesList.Items[j]);
      if j < pWorldBSP.Nodes then
        WLogStr('| | Node #' + IntToStr(j))
      else
        WLogStr('| | RootNode');
      //WLogInt('| | Flags', pWorldNode.Flags);
      //WLogInt('| | PlaneType', pWorldNode.PlaneType);
      WLogInt('| | UnknownCardinal1', pWorldNode.UnknownCardinal1);
      WLogInt('| | PolyIndex', pWorldNode.Poly);
      WLogInt('| | LeafIndex', pWorldNode.Leaf);
      WLogInt('| | Sides[0]', pWorldNode.Sides[0]);
      WLogInt('| | Sides[1]', pWorldNode.Sides[1]);
      WLogInt('| | SidesStatus[0]', pWorldNode.SidesStatus[0]);
      WLogInt('| | SidesStatus[1]', pWorldNode.SidesStatus[1]);
      WLogReal('| | UnknownFloat1', pWorldNode.UnknownFloat1);
      WLogReal('| | UnknownFloat2', pWorldNode.UnknownFloat2);
      WLogReal('| | UnknownFloat3', pWorldNode.UnknownFloat3);
      WLogReal('| | UnknownFloat4', pWorldNode.UnknownFloat4);
      WLogStr('| | ----------------------------------');
    end;

    if pWorldBSP.Leafs > 0 then
    for j := 0 to pWorldBSP.Leafs - 1 do
    begin
      pLeaf := TLTLeaf(pWorldBSP.LeafsList.Items[j]);
      WLogStr('| | Leaf #' + IntToStr(j));
      WLogInt('| | NumLeafLists', pLeaf.m_nNumLeafLists);
      WLogInt('| | LeafListIndex', pLeaf.m_nLeafListIndex);
      if (pLeaf.m_nNumLeafLists > 0) and (pLeaf.m_nNumLeafLists < $FFFF) then
      for k := 0 to pLeaf.m_nNumLeafLists - 1 do
      begin
        pLeafList := TLTLeafList(pLeaf.m_pLeafLists.Items[k]);
        WLogStr('| | | LeafList #' + IntToStr(k));
        WLogInt('| | | PortalId', pLeafList.m_nPortalId);
        WLogInt('| | | Size', pLeafList.m_nSize);
        if pLeafList.m_nSize > 0 then
        begin
          szDump := '';
          for l := 0 to pLeafList.m_nSize - 1 do
          begin
            szDump:= szDump + IntToStr(pLeafList.m_pContents[l]) + ', ';
          end;
          SetLength(szDump, Length(szDump) - 2);
          WLogStr('| | | | Contents = [ ' + szDump + ' ]');
        end;
        WLogStr('| | | ----------------------------------');
      end;

      WLogInt('| | PoliesCount', pLeaf.m_nPoliesCount);
      if pLeaf.m_nPoliesCount > 0 then
      begin
        szDump := '';
        for k := 0 to pLeaf.m_nPoliesCount - 1 do
        begin
          szDump:= szDump + ' [' + IntToStr(pLeaf.m_pPolies[k * 2]) +
                                 ', ' + IntToStr(pLeaf.m_pPolies[k * 2 + 1]) + ']';
        end;
      end;
      WLogStr('| | | Polies =' + szDump);
      WLogReal('| | Float1', pLeaf.m_fFloat1);
      WLogStr('| | ----------------------------------');
    end;

    if pWorldBSP.UserPortals > 0 then
    for j := 0 to pWorldBSP.UserPortals - 1 do
    begin
      pUserPortal := TLTUserPortal(pWorldBSP.UserPortalList.Items[j]);
      WLogStr('| | UserPortal #' + IntToStr(j));
      WLogStr('| | | Name = ' + pUserPortal.m_szName);
      WLogInt('| | Cardinal1', pUserPortal.m_nCardinal1);
      //WLogInt('| | Cardinal2', pUserPortal.m_nCardinal2);
      WLogInt('| | Word1', pUserPortal.m_nWord1);
      WLogVec('| | Center', @pUserPortal.m_vCenter);
      WLogVec('| | Dims', @pUserPortal.m_vDims);
      WLogStr('| | ----------------------------------');
    end;

    pPBlockTable := pWorldBSP.PBlockTable;
    WLogStr('| | PBlockTable');
    WLogInt('| | Cardinal1', pPBlockTable.m_nCardinal1);
    WLogInt('| | Cardinal2', pPBlockTable.m_nCardinal2);
    WLogInt('| | Cardinal3', pPBlockTable.m_nCardinal3);
    WLogInt('| | Size', pPBlockTable.m_nSize);
    WLogVec('| | Vector1', @pPBlockTable.m_vVector1);
    WLogVec('| | Vector2', @pPBlockTable.m_vVector2);
    if pPBlockTable.m_nSize > 0 then
    for k := 0 to pPBlockTable.m_nSize - 1 do
    begin
      pPBlockRecord :=  TLTPBlockRecord(pPBlockTable.m_pRecords[k]);
      WLogStr('| | | Record #' + IntToStr(k));
      WLogInt('| | | Size', pPBlockRecord.m_nSize);
      WLogInt('| | | Word1', pPBlockRecord.m_nWord1);
      if pPBlockRecord.m_nSize > 0 then
      begin
        szDump := '';
        for l := 0 to pPBlockRecord.m_nSize - 1 do
        begin
          szDump:= szDump + ' [' + IntToStr(pPBlockRecord.m_pContents[l * 6]) +
                                 ', ' + IntToStr(pPBlockRecord.m_pContents[l * 6 + 1]) +
                                 ', ' + IntToStr(pPBlockRecord.m_pContents[l * 6 + 2]) +
                                 ', ' + IntToStr(pPBlockRecord.m_pContents[l * 6 + 3]) +
                                 ', ' + IntToStr(pPBlockRecord.m_pContents[l * 6 + 4]) +
                                 ', ' + IntToStr(pPBlockRecord.m_pContents[l * 6 + 5]) + ']';
        end;
        WLogStr('| | | | Contents =' + szDump);
      end;
      WLogStr('| | | ----------------------------------');
    end;
    WLogStr('| | ----------------------------------');

    {WLogStr('| | UnknownStruct after Nodes');
    WLogInt('| | Cardinal1', pWorldBSP.UnknownStruct.n1);
    WLogInt('| | Cardinal2', pWorldBSP.UnknownStruct.n2);
    WLogInt('| | Cardinal3', pWorldBSP.UnknownStruct.n3);
    WLogVec('| | Vector1', @pWorldBSP.UnknownStruct.v1);
    WLogVec('| | Vector2', @pWorldBSP.UnknownStruct.v2); }

    WLogStr('| ----------------------------------');

    m_pMemoryStream.Position := pWorldData.NextPos;
    i := i + 1;
  end;

  g_nGlobalLogIndex := LOG_WORLD_TREE;
  m_pMemoryStream.Read(WorldExtentsSH.vBoxMin, 12);
  m_pMemoryStream.Read(WorldExtentsSH.vBoxMax, 12);

  WLogStr('| ---- WorldExtents');
  WLogVec('| BoxMin', @WorldExtentsSH.vBoxMin);
  WLogVec('| BoxMax', @WorldExtentsSH.vBoxMax);
  WLogStr('| ----------------------------------');

  WLogStr('-----------------------------------');
  WriteLn('ReadPos: ' + IntToHex(m_pMemoryStream.Position, 8));
end;

procedure TLTWorldReader.ReadRenderData(bSave: Boolean);
var i, j, k, n: Cardinal;
    anTempArray: TDynByteArray;
    nLMType: Cardinal = 0;
    nBatches: Byte = 0;
    nFrames: Word = 0;
    pAnim: TLMAnim;
    pPolyRef: TLMPolyRef;
    pBatch: TLMBatch;
    pFrame: TLMFrame;
    pPolyData: TLMFramePolyData;
    nNameLen: Word = 0;
    szAnimName: string;
    slAnimList: TStringList;
    szDump: string;
begin
  slAnimList := TStringList.Create;
  SetLength(anTempArray, LIGHTMAP_MAX_DATA_SIZE);

  WLogStr('---- RenderData');

  m_pMemoryStream.Read(WorldLMAnimList.nTotalFrames1, 4);
  m_pMemoryStream.Read(WorldLMAnimList.nTotalAnims, 4);
  m_pMemoryStream.Read(WorldLMAnimList.nTotalMemory, 4);
  m_pMemoryStream.Read(WorldLMAnimList.nTotalFrames2, 4);
  m_pMemoryStream.Read(WorldLMAnimList.nNumLMAnims, 4);

  WLogInt('TotalFrames', WorldLMAnimList.nTotalFrames1);
  WLogInt('TotalAnims', WorldLMAnimList.nTotalAnims);
  WLogInt('TotalMemory', WorldLMAnimList.nTotalMemory);
  WLogInt('TotalFrames', WorldLMAnimList.nTotalFrames2);
  WLogInt('NumLMAnims', WorldLMAnimList.nNumLMAnims);

  if WorldLMAnimList.nNumLMAnims > 0 then
  begin
    for i := 0 to WorldLMAnimList.nNumLMAnims - 1 do
    begin
      WLogAddr('LMAnim starts', m_pMemoryStream.Position);
      pAnim := TLMAnim.Create;

      // LMAnim, LMPolyRef, LMFrame, uchar, LMFramePolyData
      m_pMemoryStream.Read(nNameLen, 2);
      SetLength(szAnimName, nNameLen);
      m_pMemoryStream.Read(szAnimName[1], nNameLen);
      pAnim.Name := szAnimName;

      WriteLn('--- Reading LA: ', szAnimName);

      if g_bLMFramesToSeparateTGA then
        CreateDir(CPData.DumpsDir + CPData.Sep + szAnimName);

      m_pMemoryStream.Read(nLMType, 4);
      m_pMemoryStream.Read(nBatches, 1);
      m_pMemoryStream.Read(nFrames, 2);

      //if nBatches > 1 then
      //  WLogStrWarn(Format('LMAnim "%s" has %d batches!', [szAnimName, nBatches]));


      pAnim.Batches := nBatches;
      pAnim.Frames := nFrames;
      pAnim.LMType := nLMType;

      WLogStr('| ' + pAnim.Name);
      WLogInt('| Batches', pAnim.Batches);
      WLogInt('| Frames', pAnim.Frames);
      WLogInt('| LMType', pAnim.LMType);

      // Somekind of indices? LMPolyRef?
      if pAnim.Frames > 0 then
      begin
        for j := 0 to pAnim.Frames - 1 do
        begin
          pPolyRef := TLMPolyRef.Create;
          m_pMemoryStream.Read(pPolyRef.m_nModelIndex, 2);
          m_pMemoryStream.Read(pPolyRef.m_nPolyIndex, 2);
          AssignWorldPolyToLMPolyRef(pPolyRef);
          pAnim.PolyRefsList.Add(pPolyRef);
          WLogStr('| | FrameIndices #' + Format('%d [%d, %d]', [j, pPolyRef.m_nModelIndex, pPolyRef.m_nPolyIndex]));
        end;
      end;

      if pAnim.Batches > 0 then
      begin

        for j := 0 to pAnim.Batches - 1 do
        begin
          pBatch := TLMBatch.Create;

          WLogStr('| | Batch #' + IntToStr(j));

          // Main geometry lightmaps? LMFrame?
          if pAnim.Frames > 0 then
          begin
            for k := 0 to pAnim.Frames - 1 do
            begin

              WLogStr('| | | FrameLightmap #' + IntToStr(k));

              pFrame := TLMFrame.Create;
              AssignWidthAndHeightToLMFrame(pAnim, k, pFrame);

              m_pMemoryStream.Read(pFrame.m_nSize, 2);

              // Skip zero length
              if pFrame.m_nSize > 0 then
              begin
                //if pFrame.m_nSize < 4 then
                //  Logger.WLog(LM_WARN, 'LMFrame[' + IntToStr(k) + '] has non-zero length < 4');

                SetLength(pFrame.m_anData, pFrame.m_nSize);
                m_pMemoryStream.Read(pFrame.m_anData[0], pFrame.m_nSize);

                if pAnim.LMType > 0 then
                begin
                  pFrame.m_nDecSize := DecompressShadowMapDBG(pFrame.m_anData, pFrame.m_nSize, anTempArray);
                  SetLength(pFrame.m_anDecData, pFrame.m_nDecSize * 4);
                  ExpandShadowMap(anTempArray, pFrame.m_nDecSize, pFrame.m_anDecData, GetObjectDWordColor(pAnim.Name));
                  pFrame.m_nDecSize := pFrame.m_nDecSize * 4;

                  // compression test {
                  {pFrame.m_nReSize := CompressShadowMapDBG(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, anTempArray);
                  if pFrame.m_nReSize <> pFrame.m_nSize then
                  begin
                    WriteLn('pFrame.m_nReSize <> pFrame.m_nSize');
                  end
                  else
                  begin
                    if CompareDynArrays(pFrame.m_anData, anTempArray, pFrame.m_nSize) > -1 then
                      WriteLn('pFrame.m_nReSize = pFrame.m_nSize, but data differs!');
                  end;}
                  // compression test }
                end
                else
                begin
                  pFrame.m_nDecSize := DecompressLMData(pFrame.m_anData, pFrame.m_nSize, anTempArray);
                  SetLength(pFrame.m_anDecData, pFrame.m_nDecSize);
                  Move(anTempArray[0], pFrame.m_anDecData[0], pFrame.m_nDecSize);

                  // compression test {
                  {pFrame.m_nReSize := CompressLMData(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, anTempArray);
                  if pFrame.m_nReSize <> pFrame.m_nSize then
                  begin
                    WriteLn('pFrame.m_nReSize <> pFrame.m_nSize');
                  end
                  else
                  begin
                    if CompareDynArrays(pFrame.m_anData, anTempArray, pFrame.m_nSize) > -1 then
                      WriteLn('pFrame.m_nReSize = pFrame.m_nSize, but data differs!');
                  end; }
                  // compression test }
                end;

                if pFrame.m_nDecSize <> pFrame.m_nWidth * pFrame.m_nHeight * 4 then
                begin
                  WLogStrWarn(Format('Discrepancy in LMFrame[%d] uncompressed size! Uncompressed: %d, Predicted: %d', [k, pFrame.m_nDecSize, pFrame.m_nWidth * pFrame.m_nHeight * 4]));
                end;

                pFrame.SwapRB;

                // test
                if bSave and g_bLMFramesToSeparateTGA then
                  SaveArrayToTGA(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, CPData.DumpsDir + CPData.Sep + szAnimName + CPData.Sep + IntToStr(k) + '.tga', 4, True, False);

              end;
              pBatch.FramesList.Add(pFrame);

              WLogStr('| | | | Size = ' + Format('%d (%d)', [pFrame.m_nSize, pFrame.m_nDecSize]));
              WLogStr('| | | | Dims = ' + Format('%d x %d', [pFrame.m_nWidth, pFrame.m_nHeight]));
              WLogStr('| | | | Data = ' + DynArray_Sha1Hash(pFrame.m_anData, pFrame.m_nSize));
              WLogStr('| | | | DecData = ' + DynArray_Sha1Hash(pFrame.m_anDecData, pFrame.m_nDecSize));
              WLogStr('| | | ----------------------------------');
            end;
          end;
        //end;

        if WorldHeader.nVersion = 70 then
        begin
          // WorldModels vertex colors? LMFramePolyData?
          if pAnim.Frames > 0 then
          begin
            for k := 0 to pAnim.Frames - 1 do
            begin
              WLogStr('| | | FramePolyData #' + IntToStr(k));

              pPolyData := TLMFramePolyData.Create;

              m_pMemoryStream.Read(pPolyData.m_nVertices, 1);
              if pPolyData.m_nVertices > 0 then
              begin

                SetLength(pPolyData.m_anData, pPolyData.m_nVertices * 3);

                for n := 0 to pPolyData.m_nVertices - 1 do
                  m_pMemoryStream.Read(pPolyData.m_anData[n * 3 + 0], 1); // R

                for n := 0 to pPolyData.m_nVertices - 1 do
                  m_pMemoryStream.Read(pPolyData.m_anData[n * 3 + 1], 1); // G

                for n := 0 to pPolyData.m_nVertices - 1 do
                  m_pMemoryStream.Read(pPolyData.m_anData[n * 3 + 2], 1); // B

                // Red
                {for n := 0 to pPolyData.m_nVertices - 1 do
                begin
                  m_pMemoryStream.Read(pPolyData.m_anR[n], 1);
                end;
                // Green
                for n := 0 to pPolyData.m_nVertices - 1 do
                begin
                  m_pMemoryStream.Read(pPolyData.m_anG[n], 1);
                end;
                // Blue
                for n := 0 to pPolyData.m_nVertices - 1 do
                begin
                  m_pMemoryStream.Read(pPolyData.m_anB[n], 1);
                end; }

              end;

              WLogInt('| | | Vertices', pPolyData.m_nVertices);

              szDump := '';

              for n := 0 to pPolyData.m_nVertices - 1 do
              begin
                szDump := szDump + ' [' + IntToStr(pPolyData.m_anData[n * 3 + 0]) +
                       ' ' + IntToStr(pPolyData.m_anData[n * 3 + 1]) +
                       ' ' + IntToStr(pPolyData.m_anData[n * 3 + 2]) + ']';
              end;
              WLogStr('| | | | PoliesData =' + szDump);

              pBatch.FramePolyDataList.Add(pPolyData);
              WLogStr('| | | ----------------------------------');
            end;
          end;

        end;

        pAnim.BatchesList.Add(pBatch);
        WLogStr('| | ----------------------------------');
        end;
      end;

      WorldLMAnimList.pLMAnimList.Add(pAnim);
      slAnimList.Add(pAnim.Name);
      if bSave then
        pAnim.SaveBatchesOnDisk;

      WLogStr('| ----------------------------------');
    end;
  end;

  WLogStr('----------------------------------');
  SetLength(anTempArray, 0);
  slAnimList.SaveToFile(CPData.DumpsDir + CPData.Sep + 'LightAnimList.txt');
  slAnimList.Free;
  //m_pMemoryStream.SaveToFile('1output.dat');
  //Sleep(50000);
end;

procedure TLTWorldReader.WriteRenderData;
var nTempCardinal: Cardinal = 0;
    i, j, k, n: Cardinal;
    slLightAnims: TStringList;
    pAnim: TLMAnim;
    anTempArray: TDynByteArray;
    pBatch: TLMBatch;
    pFrame: TLMFrame;
    pPolyRef: TLMPolyRef;
    pPolyData: TLMFramePolyData;
    szAnimName: string;
begin
  SetLength(anTempArray, LIGHTMAP_MAX_DATA_SIZE);

  m_pMemoryStream.Read(nTempCardinal, 4);
  m_pMemoryStream.Read(nTempCardinal, 4);
  m_pMemoryStream.Read(nTempCardinal, 4);
  m_pMemoryStream.Read(nTempCardinal, 4);

  slLightAnims := TStringList.Create;
  slLightAnims.LoadFromFile(CPData.DumpsDir + CPData.Sep + 'LightAnimList.txt');

  m_pMemoryStream.Write(slLightAnims.Count, 4);

  for i := 0 to slLightAnims.Count - 1 do
  begin
    pAnim := TLMAnim.Create;
    pAnim.Name := slLightAnims.Strings[i];
    pAnim.LoadBatchesFromDisk;

    szAnimName := pAnim.Name;
    nTempCardinal := Length(szAnimName);

    WriteLn('--- Writing LA: ', szAnimName);

    m_pMemoryStream.Write(nTempCardinal, 2);
    m_pMemoryStream.Write(szAnimName[1], nTempCardinal);
    m_pMemoryStream.Write(pAnim.LMType, 4);
    m_pMemoryStream.Write(pAnim.Batches, 1);
    m_pMemoryStream.Write(pAnim.Frames, 2);

    if pAnim.Frames > 0 then
    begin
      for j := 0 to pAnim.Frames - 1 do
      begin
        pPolyRef := TLMPolyRef(pAnim.PolyRefsList.Items[j]);
        m_pMemoryStream.Write(pPolyRef.m_nModelIndex, 2);
        m_pMemoryStream.Write(pPolyRef.m_nPolyIndex, 2);
      end;
    end;

    for j := 0 to pAnim.Batches - 1 do
    begin
      pBatch := TLMBatch(pAnim.BatchesList.Items[j]);

      if pAnim.Frames > 0 then
      begin
        for k := 0 to pAnim.Frames - 1 do
        begin
          pFrame := TLMFrame(pBatch.FramesList.Items[k]);

          if pFrame.m_nDecSize > 0 then
          begin
            pFrame.SwapRB;

            if pAnim.LMType > 0 then
              pFrame.m_nSize := CompressShadowMapDBG(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, anTempArray, False)
            else
              pFrame.m_nSize := CompressLMData(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, anTempArray);

            SetLength(pFrame.m_anData, pFrame.m_nSize);
            Move(anTempArray[0], pFrame.m_anData[0], pFrame.m_nSize);

            m_pMemoryStream.Write(pFrame.m_nSize, 2);
            m_pMemoryStream.Write(pFrame.m_anData[0], pFrame.m_nSize);
          end
          else
          begin
            pFrame.m_nSize := pFrame.m_nDecSize;
            m_pMemoryStream.Write(pFrame.m_nSize, 2);
          end;

        end;
      end;

      if WorldHeader.nVersion = 70 then
      begin
        if pAnim.Frames > 0 then
        begin

          for k := 0 to pAnim.Frames - 1 do
          begin
            pPolyData := TLMFramePolyData(pBatch.FramePolyDataList.Items[k]);
            m_pMemoryStream.Write(pPolyData.m_nVertices, 1);

            if pPolyData.m_nVertices > 0 then
            begin
              for n := 0 to pPolyData.m_nVertices - 1 do
                m_pMemoryStream.Write(pPolyData.m_anData[n * 3 + 0], 1); // R

              for n := 0 to pPolyData.m_nVertices - 1 do
                m_pMemoryStream.Write(pPolyData.m_anData[n * 3 + 1], 1); // G

              for n := 0 to pPolyData.m_nVertices - 1 do
                m_pMemoryStream.Write(pPolyData.m_anData[n * 3 + 2], 1); // B
            end;
          end;

        end;
      end;

    end;
  end;

  SetLength(anTempArray, 0);
  slLightAnims.Free;
  m_pMemoryStream.SaveToFile(m_szFilename + '_LAUPDATED.dat');
end;

procedure TLTWorldReader.AssignWorldPolyToLMPolyRef(pPolyRef: TLMPolyRef);
var pWorldModel: TLTWorldBsp;
begin
  pWorldModel := TLTWorldData(WorldModelList.pModelList.Items[pPolyRef.m_nModelIndex]).OriginalBSP;
  pPolyRef.m_pWorldPoly := TLTWorldPoly(pWorldModel.PoliesList.Items[pPolyRef.m_nPolyIndex]);
end;

procedure TLTWorldReader.AssignWidthAndHeightToLMFrame(pAnim: TLMAnim; nIndex: Cardinal; pFrame: TLMFrame);
var pPoly: TLTWorldPoly;
begin
  pPoly := TLMPolyRef(pAnim.PolyRefsList.Items[nIndex]).m_pWorldPoly;
  pFrame.m_nWidth := pPoly.LightmapWidth;
  pFrame.m_nHeight := pPoly.LightmapHeight;

  // temp
  pPoly.LMFrameIndex := nIndex;
end;

procedure TLTWorldReader.ReadWorld;
begin
  g_nGlobalLogIndex := LOG_HEADER;
  ReadHeader;
  ReadPropertiesAndExtents;
  //g_nGlobalLogIndex := LOG_WORLD_TREE;
  //ReadWorldTree;
  m_pMemoryStream.Position := WorldHeader.dwBspDataPos;
  g_nGlobalLogIndex := LOG_WORLD_MODELS;
  ReadWorldModels;

  m_pMemoryStream.Position := WorldHeader.dwObjectDataPos;
  g_nGlobalLogIndex := LOG_OBJECTS;
  ReadObjects;
  g_nGlobalLogIndex := LOG_RENDER_DATA;
  m_pMemoryStream.Position := WorldHeader.dwRenderDataPos;
  if g_szLightAnimsJob = 'save' then
  begin
    ReadRenderData(True);
  end
  else if g_szLightAnimsJob = 'read' then
  begin
    ReadRenderData(False);
  end
  else if g_szLightAnimsJob = 'load' then
  begin
    m_pMemoryStream.SetSize(WorldHeader.dwRenderDataPos + 16);
    WriteRenderData;
  end;
  g_nGlobalLogIndex := LOG_GENERIC;
end;

procedure TLTWorldReader.RemoveWorldModel(szName: string);
var i: Cardinal;
    pModel: TLTWorldBsp;
begin
  for i := 0 to WorldModelList.nNumModels - 1 do
  begin
     pModel := TLTWorldData(WorldModelList.pModelList.Items[i]).OriginalBSP;
     if pModel.WorldName = szName then
     begin
       WorldModelList.pModelList.Delete(i);
       Dec(WorldModelList.nNumModels, 1);
       Exit;
     end;
  end;
end;

procedure TLTWorldReader.RemoveWorldObject(szName: string);
var i: Cardinal;
    pObject: TLTWorldObject;
begin
  for i := 0 to ObjectList.nNumObjects - 1 do
  begin
    pObject := TLTWorldObject(WorldObjectList.pObjectList.Items[i]);
    if pObject.GetObjectName = szName then
    begin
      WorldObjectList.pObjectList.Delete(i);
      Dec(WorldObjectList.nNumObjects, 1);
      Exit;
    end;
  end;
end;

function TLTWorldReader.FindWorldObject(szName: string): TLTWorldObject;
var i: Cardinal;
    pObject: TLTWorldObject;
begin
  for i := 0 to ObjectList.nNumObjects - 1 do
  begin
    pObject := TLTWorldObject(WorldObjectList.pObjectList.Items[i]);
    if pObject.GetObjectName = szName then
    begin
      Exit(pObject);
    end;
  end;
end;

function TLTWorldReader.GetObjectDWordColor(szLightAnimName: string): Cardinal;
var szObjectName: string;
    vColor: LTVector;
begin
  szObjectName := Copy(szLightAnimName, 1, Length(szLightAnimName) - 4);
  vColor := FindWorldObject(szObjectName).GetProperty('Color').DataVec;
  Result := (Trunc(vColor.z)) + (Trunc(vColor.y) shl 8) + (Trunc(vColor.x) shl 16);
end;

procedure TLTWorldReader.MoveWorldModel(szName: string; nToIndex: Integer);
var i: Cardinal;
    pModel: TLTWorldBsp;
begin
  for i := 0 to WorldModelList.nNumModels - 1 do
  begin
     pModel := TLTWorldData(WorldModelList.pModelList.Items[i]).OriginalBSP;
     if pModel.WorldName = szName then
     begin
       WorldModelList.pModelList.Move(i, nToIndex);
       Exit;
     end;
  end;
end;


end.
