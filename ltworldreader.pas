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
    procedure ReadRenderData;

    procedure AssignWorldPolyToLMPolyRef(pPolyRef: TLMPolyRef);
    procedure AssignWidthAndHeightToLMFrame(pAnim: TLMAnim; nIndex: Cardinal;
      pFrame: TLMFrame);

  protected
  public
    procedure ReadWorld;
    procedure RemoveWorldModel(szName: string);
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
      WLogInt('| | LightmapWidth', pWorldPoly.LightmapWidth);
      WLogInt('| | LightmapHeight', pWorldPoly.LightmapHeight);
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

    {WLogStr('| | UnknownStruct after Nodes');
    WLogInt('| | Cardinal1', pWorldBSP.UnknownStruct.n1);
    WLogInt('| | Cardinal2', pWorldBSP.UnknownStruct.n2);
    WLogInt('| | Cardinal3', pWorldBSP.UnknownStruct.n3);
    WLogVec('| | Vector1', @pWorldBSP.UnknownStruct.v1);
    WLogVec('| | Vector2', @pWorldBSP.UnknownStruct.v2); }

    WLogStr('| ----------------------------------');
    m_pMemoryStream.Position := pWorldData.NextPos;
  end;

  WLogStr('-----------------------------------');
  WLogStr('ReadPos: ' + IntToHex(m_pMemoryStream.Position, 8));
end;

procedure TLTWorldReader.ReadRenderData;
var i, j, k, n: Cardinal;
    anTempArray: TDynByteArray;
    nTempCardinal: Cardinal = 0;
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
begin
  slAnimList := TStringList.Create;
  SetLength(anTempArray, LIGHTMAP_MAX_DATA_SIZE);

  m_pMemoryStream.Read(nTempCardinal, 4);
  m_pMemoryStream.Read(nTempCardinal, 4);
  m_pMemoryStream.Read(nTempCardinal, 4);
  m_pMemoryStream.Read(nTempCardinal, 4);
  m_pMemoryStream.Read(WorldLMAnimList.nNumLMAnims, 4);

  if WorldLMAnimList.nNumLMAnims > 0 then
  begin
    for i := 0 to WorldLMAnimList.nNumLMAnims - 1 do
    begin
      pAnim := TLMAnim.Create;

      // LMAnim, LMPolyRef, LMFrame, uchar, LMFramePolyData
      m_pMemoryStream.Read(nNameLen, 2);
      SetLength(szAnimName, nNameLen);
      m_pMemoryStream.Read(szAnimName[1], nNameLen);
      pAnim.Name := szAnimName;

      m_pMemoryStream.Read(nTempCardinal, 4);
      m_pMemoryStream.Read(nBatches, 1);
      m_pMemoryStream.Read(nFrames, 2);

      pAnim.Batches := nBatches;
      pAnim.Frames := nFrames;

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
        end;
      end;

      if pAnim.Batches > 0 then
      begin
        for j := 0 to pAnim.Batches - 1 do
        begin
          pBatch := TLMBatch.Create;

          // Main geometry lightmaps? LMFrame?
          if pAnim.Frames > 0 then
          begin
            for k := 0 to pAnim.Frames - 1 do
            begin
              pFrame := TLMFrame.Create;
              AssignWidthAndHeightToLMFrame(pAnim, k, pFrame);

              m_pMemoryStream.Read(pFrame.m_nSize, 2);
              // Skip zero length
              if pFrame.m_nSize > 0 then
              begin
                SetLength(pFrame.m_anData, pFrame.m_nSize);
                m_pMemoryStream.Read(pFrame.m_anData[0], pFrame.m_nSize);

                pFrame.m_nDecSize := DecompressLMData(pFrame.m_anData, pFrame.m_nSize, anTempArray{%H-});
                if pFrame.m_nDecSize <> pFrame.m_nWidth * pFrame.m_nHeight * 4 then
                begin
                  Logger.WLog(LM_WARN, Format('Discrepancy in LMFrame[%d] uncompressed size! Uncompressed: %d, Predicted: %d', [k, pFrame.m_nDecSize, pFrame.m_nWidth * pFrame.m_nHeight * 4]));
                end;
                SetLength(pFrame.m_anDecData, pFrame.m_nDecSize);
                Move(anTempArray[0], pFrame.m_anDecData[0], pFrame.m_nDecSize);

                // Test
                //FillByte(anTempArray[0], 4096, 0);
                //pFrame.m_nReSize := CompressLMData(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, anTempArray);
                //SetLength(pFrame.m_anReData, pFrame.m_nReSize);
                //Move(anTempArray[0], pFrame.m_anReData[0], pFrame.m_nReSize);

                pFrame.SwapRB;

                //if szAnimName = 'StarLightView_Pred1__SV' then
                //  SaveArrayToTGA(pFrame.m_anDecData, pFrame.m_nWidth, pFrame.m_nHeight, CPData.Dir + CPData.Sep + 'dumps' + CPData.Sep + 'LM_' + pAnim.Name + '_' + IntToStr(k) + '.tga', 4, True, False);
              end;
              pBatch.FramesList.Add(pFrame);

            end;
          end;

          if WorldHeader.nVersion = 70 then
          begin

            // WorldModels vertex colors? LMFramePolyData?
            if pAnim.Frames > 0 then
            begin
              for k := 0 to pAnim.Frames - 1 do
              begin
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

                //if szAnimName = 'StarLightView_Pred1__SV' then
                //  SaveArrayToTGA(pPolyData.m_anData, pPolyData.m_nVertices, 1, CPData.Dir + CPData.Sep + 'dumps' + CPData.Sep + 'LMPD_' + pAnim.Name + '_' + IntToStr(k) + '.tga', 3, False, False);

                pBatch.FramePolyDataList.Add(pPolyData);
              end;
            end;

          end;

          pAnim.BatchesList.Add(pBatch);
        end;
      end;

      WorldLMAnimList.pLMAnimList.Add(pAnim);
      slAnimList.Add(pAnim.Name);
      pAnim.SaveBatchesOnDisk;

    end;
  end;

  SetLength(anTempArray, 0);
  slAnimList.SaveToFile(CPData.DumpsDir + CPData.Sep + 'LightAnimList.txt');
  slAnimList.Free;
  //m_pMemoryStream.SaveToFile('1output.dat');
  //Sleep(50000);
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
  if g_bReadLightAnims then
  begin
    m_pMemoryStream.Position := WorldHeader.dwRenderDataPos;
    ReadRenderData;
  end;
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