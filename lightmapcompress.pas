unit lightmapcompress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, globals;

const
  LIGHTMAP_MAX_DATA_SIZE = 4096;

function DecompressLMDataJP(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
function CompressLMData(anData: TDynByteArray; nWidth: Word; nHeight: Word; anOut: TDynByteArray): Cardinal;
function DecompressLMData(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;

implementation

function DecompressLMDataJP(anCompressed: TDynByteArray; nDataLen: Cardinal; anOut: TDynByteArray): Cardinal;
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
end;

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


end.

