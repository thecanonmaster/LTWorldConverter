unit globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MyLogger, MyCrossPlatform, ltworldtypes;

const
  LTWC_VERSION = 'v0.124 alpha';

type
  TDynByteArray = array of Byte;
  TDynWordArray = array of Word;
  TStringArray = array of string;
  TDynCardinalArray = array of Cardinal;

var
  Logger: TMyLameLogger;
  CPData: TMyCrossPlatform;
  g_szBrushType: string;
  g_szBrushGenType: string;
  g_szGeometrySource: string;
  g_bDumpNodes: Boolean;
  //g_bIgnoreObjects: Boolean;
  g_szLightAnimsJob: string;
  g_bDebugProps: Boolean;
  g_bAdditionalTexturesLTA: Boolean = False;

procedure WLogReal(S: string; F: LTFloat);
procedure WLogVec(S: string; V: PLTVector);
procedure WLogStr(S: string);
procedure WLogInt(S: string; N: cardinal);
procedure WLogAddr(S: string; N: cardinal);
procedure WLogStrWarn(S: string);
procedure SaveArrayToFile(Buffer: TDynByteArray; nSize: Cardinal; szFilename: string);
procedure SaveArrayToPPM(Buffer: TDynByteArray; nWidth: Word; nHeight: Word; szFilename: string);
procedure SaveArrayToTGA(Buffer: TDynByteArray; nWidth: Word; nHeight: Word; szFilename: string; nChannels: Byte; bSolid: Boolean; bSwapRB: Boolean);
function LoadArrayFromTGA(var Buffer: TDynByteArray; var nWidth: Word; var nHeight: Word; szFilename: string; var nChannels: Byte; bZeroAlpha: Boolean; bSwapRB: Boolean): Cardinal;
procedure DynArray_MemSet32(Buffer: TDynCardinalArray; nPosition: Cardinal; nValue: Cardinal; nCount: Cardinal);
procedure SimpleBlt32(Dest: TDynCardinalArray; Source: TDynCardinalArray; nDestWidth: Word; nSourceWidth: Word; nSourceHeight: Word; nX: Word; nY: Word);
procedure SimpleReverseBlt32(Source: TDynCardinalArray; Dest: TDynCardinalArray; nSourceWidth: Word; nDestWidth: Word; nDestHeight: Word; nX: Word; nY: Word);
function CompareDynArrays(anLeft: TDynByteArray; anRight: TDynByteArray; nLength: Cardinal): Integer;

implementation

procedure WLogReal(S: string; F: LTFloat);
begin
  Logger.WLog(LM_INFO, '| ' + S + ' = ' + FormatFloat('0.000000', F));
end;

procedure WLogVec(S: string; V: PLTVector);
begin
  Logger.WLog(LM_INFO, '| ' + S + ' = ' + LTVectorToStrC(V));
end;

procedure WLogStr(S: string);
begin
  Logger.WLog(LM_INFO, S);
end;

procedure WLogStrWarn(S: string);
begin
  Logger.WLog(LM_WARN, S);
end;

procedure WLogInt(S: string; N: cardinal);
begin
  Logger.WLog(LM_INFO, '| ' + S + ' = ' + IntToStr(N));
end;

procedure WLogAddr(S: string; N: cardinal);
begin
  Logger.WLog(LM_INFO, '| ' + S + ' = $' + IntToHex(N, 8));
end;

procedure SaveArrayToFile(Buffer: TDynByteArray; nSize: Cardinal; szFilename: string);
var MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  MS.Write(Buffer[0], nSize);
  MS.SaveToFile(szFilename);
  MS.Free;
end;

procedure SaveArrayToPPM(Buffer: TDynByteArray; nWidth: Word; nHeight: Word; szFilename: string);
var MS: TMemoryStream;
    szHeader: string;
    i, j: Cardinal;
begin
  MS := TMemoryStream.Create;
  szHeader := Format('P6'#10'%d %d'#10'255'#10, [nWidth, nHeight]);
  MS.Write(szHeader[1], Length(szHeader));
  j := 0;
  for i := 0 to nWidth * nHeight {%H-}- 1 do
  begin
    MS.Write(Buffer[i * 4 + j], 3);
    j := 0;
  end;
  MS.SaveToFile(szFilename);
  MS.Free;
end;

procedure SaveArrayToTGA(Buffer: TDynByteArray; nWidth: Word; nHeight: Word; szFilename: string; nChannels: Byte; bSolid: Boolean; bSwapRB: Boolean);
var MS: TMemoryStream;
    anHeader: array[0..17] of Byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
    i, nSize: Cardinal;
begin
  anHeader[2] := 2; // TrueColor
  PWord(@anHeader[12])^ := nWidth; // 12 - 13
  PWord(@anHeader[14])^ := nHeight; // 14 - 15
  anHeader[16] := 8 * nChannels; // 32 bits
  if nChannels = 4 then
    anHeader[17] := 8 // Dunno
  else
    anHeader[17] := 0; // Dunno

  nSize := nWidth * nHeight * nChannels;

  MS := TMemoryStream.Create;
  MS.Write(anHeader[0], 18);

  i := 0;
  while i < nSize do
  begin
    if bSwapRB then
    begin
      MS.WriteByte(Buffer[i + 0]); // R
      MS.WriteByte(Buffer[i + 1]); // G
      MS.WriteByte(Buffer[i + 2]); // B
    end
    else
    begin
      MS.WriteByte(Buffer[i + 2]); // B
      MS.WriteByte(Buffer[i + 1]); // G
      MS.WriteByte(Buffer[i + 0]); // R
    end;

    if nChannels = 4 then
    begin
      if not bSolid then
        MS.WriteByte(Buffer[i + 3]) // A
      else
        MS.WriteByte($FF);
    end;

    Inc(i, nChannels);
  end;

  MS.SaveToFile(szFilename);
  MS.Free;
end;

function LoadArrayFromTGA(var Buffer: TDynByteArray; var nWidth: Word; var nHeight: Word; szFilename: string; var nChannels: Byte; bZeroAlpha: Boolean; bSwapRB: Boolean): Cardinal;
var MS: TMemoryStream;
    anHeader: array[0..17] of Byte;
    i: Cardinal;
begin
  MS := TMemoryStream.Create;
  MS.LoadFromFile(szFilename);
  MS.ReadBuffer({%H-}anHeader[0], 18);

  nWidth := PWord(@anHeader[12])^;
  nHeight := PWord(@anHeader[14])^;
  nChannels := anHeader[16] div 8;

  Result := nWidth * nHeight * nChannels;
  SetLength(Buffer, Result);

  i := 0;
  while i < Result do
  begin
    if bSwapRB then
    begin
      Buffer[i + 0] := MS.ReadByte(); // R
      Buffer[i + 1] := MS.ReadByte(); // G
      Buffer[i + 2] := MS.ReadByte(); // B
    end
    else
    begin
      Buffer[i + 2] := MS.ReadByte(); // B
      Buffer[i + 1] := MS.ReadByte(); // G
      Buffer[i + 0] := MS.ReadByte(); // R
    end;

    if nChannels = 4 then
    begin
      Buffer[i + 3] := MS.ReadByte(); // A
      if bZeroAlpha then Buffer[i + 3] := 0;
    end;

    Inc(i, nChannels);
  end;

  MS.Free;
end;

procedure SimpleBlt32(Dest: TDynCardinalArray; Source: TDynCardinalArray; nDestWidth: Word; nSourceWidth: Word; nSourceHeight: Word; nX: Word; nY: Word);
var i: Word;
begin
  for i := 0 to nSourceHeight - 1 do
  begin
    Move(Source[i * nSourceWidth], Dest[(nY * nDestWidth + nX) + (nDestWidth * i)], nSourceWidth * 4);
  end;
end;

procedure SimpleReverseBlt32(Source: TDynCardinalArray; Dest: TDynCardinalArray; nSourceWidth: Word; nDestWidth: Word; nDestHeight: Word; nX: Word; nY: Word);
var i: Word;
begin
  for i := 0 to nDestHeight - 1 do
  begin
    Move(Source[(nY * nSourceWidth + nX) + (nSourceWidth * i)], Dest[i * nDestWidth], nDestWidth * 4);
  end;
end;

function CompareDynArrays(anLeft: TDynByteArray; anRight: TDynByteArray; nLength: Cardinal): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to nLength - 1 do
  begin
    if anLeft[i] <> anRight[i] then Exit(i);
  end;
end;

procedure DynArray_MemSet32(Buffer: TDynCardinalArray; nPosition: Cardinal; nValue: Cardinal; nCount: Cardinal);
var i: Cardinal;
begin
  for i := 0 to nCount - 1 do
  begin
    Buffer[nPosition + i] := nValue;
  end;
end;

end.

