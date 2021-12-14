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
    procedure ReadProperties;
    procedure ReadObjects;
    procedure ReadWorldModels;
    procedure ReadRenderDataSH(bSave: Boolean);

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

procedure TLTWorldReader.ReadProperties;
var
  nLen: integer;
begin
  nLen := 0;
  m_pMemoryStream.Read(nLen, 4);
  SetLength(WorldProperties, nLen);
  if nLen > 0 then m_pMemoryStream.Read(WorldProperties[1], nLen);
  // now log it!
  WLogStr('---- WorldProperties and WorldExtents:');
  WLogStr('| Properties = ' + WorldProperties);
  WLogStr('-----------------------------------');
end;

procedure TLTWorldReader.ReadHeader;
begin
  m_pMemoryStream.Read(WorldHeader.nVersion, 4);
  m_pMemoryStream.Read(WorldHeader.dwObjectDataPos, 4);
  m_pMemoryStream.Read(WorldHeader.dwBspDataPos, 4);
  // now log it!
  WLogStr('---- WorldHeader:');
  WLogInt('Version', WorldHeader.nVersion);
  WLogAddr('ObjectDataPos', WorldHeader.dwObjectDataPos);
  WLogAddr('BspDataPos', WorldHeader.dwBspDataPos);
  WLogStr('-----------------------------------');
end;

procedure TLTWorldReader.ReadObjects;
var
  i, j, file_pos: Cardinal;
  WorldObject: TLTWorldObject;
begin
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
      pWorldBSP.SaveNodesDump(g_szDumpsDir + g_szPathSep + pWorldBSP.WorldName + '.ltnd');

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
      WLogInt('| | PlaneIndex', TLTWorldSurface(pWorldBSP.SurfacesList.Items[j]).m_nPlane);
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

      WLogInt('| | UnknownWord1', pWorldPoly.LightmapWidth);
      WLogInt('| | UnknownWord2', pWorldPoly.LightmapHeight);

      szDump := '';

      WLogInt('| | UnknownCardinal1', pWorldPoly.UnknownCardinal1);
      WLogInt('| | UnknownCardinal2', pWorldPoly.UnknownCardinal2);
      WLogInt('| | SurfaceIndex', pWorldPoly.Surface);

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

    pWorldData.RenderDataPos := m_pMemoryStream.Position;
    WLogInt('| RenderDataPos', pWorldData.RenderDataPos);
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
  //WriteLn('ReadPos: ' + IntToHex(m_pMemoryStream.Position, 8));
end;

procedure TLTWorldReader.ReadRenderDataSH(bSave: Boolean);
var
  slAnimList: TStringList;
  i, j: Cardinal;
  pWorldData: TLTWorldData;
  pWorldBsp: TLTWorldBsp;
  pPoly: TLTWorldPoly;
  pSurface: TLTWorldSurface;
  szNiceName: string;
  pAnim: TLMAnimSH;
  pPolyData: TLMPolyDataSH;
  bLightmapped: Boolean;
begin
  slAnimList := TStringList.Create;
  WLogStr('---- RenderData');

  WorldLMAnimList.nNumLMAnims := WorldModelList.nNumModels;

  for i := 0 to WorldLMAnimList.nNumLMAnims - 1 do
  begin

    pWorldData := TLTWorldData(WorldModelList.pModelList.Items[i]);
    pWorldBsp := pWorldData.OriginalBSP;

    if pWorldBsp.WorldName = '' then
      szNiceName := 'null'
    else
      szNiceName := pWorldBsp.WorldName;

    if g_bLMFramesToSeparateTGA then
    begin
      CreateDir(g_szDumpsDir + g_szPathSep + szNiceName)
    end;

    m_pMemoryStream.Position := pWorldData.RenderDataPos;
    WLogAddr('LMAnim starts', m_pMemoryStream.Position);
    WriteLn('--- Reading LA: ', szNiceName);

    pAnim := TLMAnimSH.Create;
    pAnim.Name := szNiceName;
    m_pMemoryStream.Read(pAnim.m_nUnknown1, 4);

    WLogStr('| ' + szNiceName);
    WLogInt('| UnknownCardinal1', pAnim.m_nUnknown1);

    for j := 0 to pWorldBsp.Polies - 1 do
    begin
      pPolyData := TLMPolyDataSH.Create;
      m_pMemoryStream.Read(pPolyData.m_vUnknown, SizeOf(LTVector));
      pAnim.PolyDataList.Add(pPolyData);
      WLogStr('| | Poly #' + IntToStr(j) + ' Unknown = ' + LTVectorToStrC(@pPolyData.m_vUnknown));
    end;

    m_pMemoryStream.Read(pAnim.m_nUnknown2, 4);
    WLogInt('| UnknownCardinal2', pAnim.m_nUnknown2);

    for j := 0 to pWorldBsp.Polies - 1 do
    begin
      pPoly := TLTWorldPoly(pWorldBsp.PoliesList.Items[j]);
      pSurface := TLTWorldSurface(pWorldBsp.SurfacesList.Items[pPoly.Surface]);
      pPolyData := TLMPolyDataSH(pAnim.PolyDataList.Items[j]);

      bLightmapped := ((pSurface.m_nFlags and SURF_LIGHTMAP) > 0);

      WLogStr('| | Lightmap #' + IntToStr(j));

      if bLightmapped then
      begin
        m_pMemoryStream.Read(pPolyData.m_nLMWidth, 1);
        m_pMemoryStream.Read(pPolyData.m_nLMHeight, 1);

        pPolyData.m_nLMSize := pPolyData.m_nLMWidth * pPolyData.m_nLMHeight * 2;
        SetLength(pPolyData.m_anLMData, pPolyData.m_nLMSize);

        if pPolyData.m_nLMSize > 0 then
        begin
          m_pMemoryStream.Read(pPolyData.m_anLMData[0], pPolyData.m_nLMSize);

          if bSave and g_bLMFramesToSeparateTGA then
            SaveArrayToTGA(pPolyData.m_anLMData, pPolyData.m_nLMWidth, pPolyData.m_nLMHeight, g_szDumpsDir + g_szPathSep + szNiceName + g_szPathSep + pWorldBsp.WorldName + '_' + IntToStr(j) + '.tga', 4, True, False);
        end
        else
        begin
          WLogStrWarn('Poly is flagged as lightmapped but lightmap size is zero!');
        end;

      end;

      WLogStr('| | | Flag = ' + BoolToStr(bLightmapped, True));
      WLogStr('| | | Dims = ' + Format('%d x %d', [pPolyData.m_nLMWidth, pPolyData.m_nLMHeight]));
      WLogStr('| | | Size = ' + IntToStr(pPolyData.m_nLMSize));
      WLogStr('| | | Data = ' + DynArray_Sha1Hash(pPolyData.m_anLMData, pPolyData.m_nLMSize));

      if (not bLightmapped) and (pPolyData.m_nLMSize > 0) then
        WLogStrWarn('Poly is not flagged as lightmapped but lightmap is present!');

      WLogStr('| ----------------------------------');

    end;

    WorldLMAnimList.pLMAnimList.Add(pAnim);
    slAnimList.Add(pWorldBsp.WorldName);
    if bSave then
      pAnim.SaveOnDisk;
    //Exit;
  end;


  WLogStr('----------------------------------');
  slAnimList.SaveToFile(g_szDumpsDir + g_szPathSep + 'LightAnimList.txt');
  slAnimList.Free;
end;


procedure TLTWorldReader.ReadWorld;
begin
  g_nGlobalLogIndex := LOG_HEADER;
  ReadHeader;
  ReadProperties;

  m_pMemoryStream.Position := WorldHeader.dwBspDataPos;
  g_nGlobalLogIndex := LOG_WORLD_MODELS;
  ReadWorldModels;

  m_pMemoryStream.Position := WorldHeader.dwObjectDataPos;
  g_nGlobalLogIndex := LOG_OBJECTS;
  ReadObjects;
  g_nGlobalLogIndex := LOG_RENDER_DATA;
  //m_pMemoryStream.Position := WorldHeader.dwRenderDataPos;
  if g_szLightAnimsJob = 'save' then
  begin
    ReadRenderDataSH(True);
  end
  else if g_szLightAnimsJob = 'read' then
  begin
    ReadRenderDataSH(False);
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
