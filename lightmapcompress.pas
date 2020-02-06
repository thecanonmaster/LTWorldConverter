unit lightmapcompress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globals;

const
  LIGHTMAP_MAX_DATA_SIZE = 4096;

//function DecompressLMDataJP(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
function CompressLMData(anData: TDynByteArray; nWidth: Word; nHeight: Word; anOut: TDynByteArray): Cardinal;
function CompressShadowMapDBG(anData: TDynByteArray; nWidth: Word; nHeight: Word; anOut: TDynByteArray): Cardinal;
function DecompressLMData(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
function DecompressShadowMap(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
function DecompressShadowMapDBG(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
procedure ExpandShadowMap(anData: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray; nFillColor: Cardinal);
procedure ShrinkShadowMap(anData: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray);

implementation

{function DecompressLMDataJP(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
var nCurrPos, nRunLen, nCurrPel: Cardinal;
    nCurrOut: Cardinal;
    nTag: Byte;
    bIsRun: Boolean;
begin
  nCurrPos := 0;
  nCurrOut := 0;

  while nCurrPos < nDataLen do
  begin
    nTag := anCompressed[nCurrPos];
    Inc(nCurrPos, 1);

    bIsRun := (nTag and $80) > 0;
    nRunLen := Cardinal(nTag and $7F) + 1;

    for nCurrPel := 0 to nRunLen - 1 do
    begin
      anOut[nCurrOut + 0] := anCompressed[nCurrPos + 0];
      anOut[nCurrOut + 1] := anCompressed[nCurrPos + 1];
      anOut[nCurrOut + 2] := anCompressed[nCurrPos + 2];

      Inc(nCurrOut, 3);

      if not bIsRun then Inc(nCurrPos, 3);
    end;

    if bIsRun then Inc(nCurrPos, 3);

  end;

  Result := nCurrOut - 3;
end;  }

function CompressLMData(anData: TDynByteArray; nWidth: Word; nHeight: Word; anOut: TDynByteArray): Cardinal;
var anDWData: TDynCardinalArray;
    nCurrPos, nOutPos, nTag, i, nSize, nRunLen: Cardinal;
    nNextPos: Cardinal = 0;
begin
  if (nWidth > 32) or (nHeight > 32) then
  begin
    WLogStrWarn(Format('LM width or height larger than allowed (%d x %d)', [nWidth, nHeight]));
    Exit(0);
  end;

  nSize := nWidth * nHeight;
  if nSize = 0 then Exit(0);

  anDWData := TDynCardinalArray(anData);
  nCurrPos := 0;
  nOutPos := 0;

  while nCurrPos < nSize do
  begin
    nTag := anDWData[nCurrPos];
    i := nCurrPos + 1;

    while True do
    begin
      if (i >= nSize) or (anDWData[i] <> nTag) then
        Break;
      Inc(i);
    end;

    nRunLen := i - nCurrPos;
    nTag := nTag and $7FFFFFFF;

    if nRunLen < 2 then
    begin
      PCardinal(@anOut[nOutPos])^ := nTag;
      Inc(nOutPos, 4);
    end
    else
    begin
      if nRunLen > 255 then
        nRunLen := 255;

      nCurrPos := nCurrPos + nRunLen {%H-}- 1;
      PCardinal(@anOut[nOutPos])^ := nTag or $80000000;
      nNextPos := nOutPos + 4;
      anOut[nNextPos] := nRunLen;
      nOutPos := nNextPos + 1;
    end;
    Inc(nCurrPos, 1);
  end;
  Result := nOutPos;
end;

function CompressShadowMapDBG(anData: TDynByteArray; nWidth: Word; nHeight: Word; anOut: TDynByteArray): Cardinal;
var anDWData: TDynCardinalArray;
    i, nPixel, nSwitch, nBYValue, nOutPos: Cardinal;
begin
  if (nWidth > 32) or (nHeight > 32) then
  begin
    WLogStrWarn(Format('ShadowMap width or height larger than allowed (%d x %d)', [nWidth, nHeight]));
    Exit(0);
  end;

  anDWData := TDynCardinalArray(anData);
  nSwitch := 0;
  nBYValue := 0;
  nOutPos := 0;

  for i := 0 to nWidth * nHeight {%H-}- 1 do
  begin
    if anDWData[i] <> 0 then nPixel := 1 else nPixel := 0;
    if nPixel = nSwitch then
    begin
      Inc(nBYValue, 1);
      if nBYValue = 255 then
      begin
        anOut[nOutPos] := 255;
        Inc(nOutPos, 1);
        if nSwitch = 0 then nSwitch := 1 else nSwitch := 0;
        nBYValue := 0;
      end;
    end
    else
    begin
      anOut[nOutPos] := nBYValue;
      Inc(nOutPos, 1);
      if nSwitch = 0 then nSwitch := 1 else nSwitch := 0;
      nBYValue := 1;
    end;
  end;

  if nBYValue > 0 then
  begin
    anOut[nOutPos] := nBYValue;
    Inc(nOutPos, 1);
  end;

  Result := nOutPos;
end;

function DecompressLMData(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
var nCurrPos, nOutPos: Cardinal;
    nBreaker: Integer;
    nTag: Cardinal;
    nCopyCount: Byte;
begin
  nBreaker := 1024;
  nCurrPos := 0;
  nOutPos := 0;
  Result := 0;

  while True do
  begin

    nTag := PCardinal(@anCompressed[nCurrPos])^;
    Inc(nCurrPos, 4);
    if (nTag and $80000000) > 0 then
    begin
      nCopyCount := anCompressed[nCurrPos];
      nTag := (nTag and $7FFFFFFF);
      Inc(nCurrPos, 1);
    end
    else
    begin
      nCopyCount := 1;
    end;

    Dec(nBreaker, nCopyCount);
    if nBreaker < 0 then
    begin
      WLogStrWarn(Format('Breaker hit during decompressing %d bytes of LM data!', [nDataLen]));
      Break;
    end;

    if nCopyCount > 0 then
    begin
      //DynArray_MemSet32(TDynCardinalArray(anOut), nOutPos div 4, nTag, nCopyCount);
      FillDWord(anOut[nOutPos], nCopyCount, nTag);
      nOutPos := nOutPos + Cardinal(4 * nCopyCount);
    end;

    if nCurrPos >= nDataLen then Exit(nOutPos);

  end;

  Result := 0;
end;

function DecompressShadowMap(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
var nCurrPos, nOutPos, nTag, nSwitch, nDelta: Cardinal;
    anMap: array[0..1] of Byte = (0, 255);
    nBYValue: Cardinal;
    nDWValue: Cardinal;
    nDWCopyCount: Cardinal;
begin
  nCurrPos := 0;
  nOutPos := 0;
  nSwitch := 0;

  while nDataLen > 0 do
  begin
    nTag := anCompressed[nCurrPos];

    if nTag > 0 then
    begin
      nBYValue := 0;
      nDWValue := 0;

      PByte(@nBYValue)^ := anMap[nSwitch];
      (PByte(@nBYValue) + 1)^ := anMap[nSwitch];
      nDWValue := nBYValue shl 16;
      PWord(@nDWValue)^ := nBYValue;
      nDWCopyCount := nTag shr 2;
      FillDWord(anOut[nOutPos], nDWCopyCount, nDWValue);
      nDelta := nOutPos + 4 * nDWCopyCount;
      FillByte(anOut[nDelta], (nTag and 3), PByte(@nBYValue)^);
      nOutPos := nDelta + (nTag and 3); //nTag;
    end;

    if nSwitch = 0 then nSwitch := 1 else nSwitch := 0;

    Dec(nDataLen, 1);
    Inc(nCurrPos, 1);
  end;

  //Move(anOut[0], anOut[nOutPos], nOutPos);
  //Move(anOut[0], anOut[nOutPos shl 1], nOutPos shl 1);
  //Result := nOutPos * 4;
  Result := nOutPos;
end;

function DecompressShadowMapDBG(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
var nCurrPos, nOutPos, nTag, nSwitch: Cardinal;
    anMap: array[0..1] of Byte = (0, 255);
    nBYValue: Byte;
begin
  nCurrPos := 0;
  nOutPos := 0;
  nSwitch := 0;

  while nDataLen > 0 do
  begin
    nTag := anCompressed[nCurrPos];

    nBYValue := anMap[nSwitch];
    FillByte(anOut[nOutPos], nTag, nBYValue);
    Inc(nOutPos, nTag);
    nTag := 0;
    if nSwitch = 0 then nSwitch := 1 else nSwitch := 0;

    Dec(nDataLen, 1);
    Inc(nCurrPos, 1);
  end;

  Result := nOutPos;
end;

procedure ExpandShadowMap(anData: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray; nFillColor: Cardinal);
var i: Cardinal;
    anDWData: TDynCardinalArray;
begin
  anDWData := TDynCardinalArray(anOut);
  for i := 0 to nDataLen - 1 do
  begin
    if anData[i] = 0 then
    begin
      anDWData[i] := 0;
    end
    else
    begin
      //anDWData[i] := (anSource[i]) + (anSource[i] shl 8) + (anSource[i] shl 16);
      anDWData[i] := nFillColor;
    end;
  end;
end;

procedure ShrinkShadowMap(anData: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray);
var i: Cardinal;
    anDWData: TDynCardinalArray;
    nPixel: Byte;
begin
  anDWData := TDynCardinalArray(anData);
  for i := 0 to nDataLen - 1 do
  begin
    nPixel := (PByte(@anDWData[i]) + 4)^;
    if nPixel > 0 then anOut[i] := $FF else anOut[i] := 0;
  end;
end;


end.

