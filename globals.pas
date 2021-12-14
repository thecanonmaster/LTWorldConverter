unit globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MyLogger, MyCrossPlatform, ltworldtypes, sha1;

const
  LTWC_VERSION = 'v0.101 alpha';

type
  TDynByteArray = array of Byte;
  TDynWordArray = array of Word;
  TStringArray = array of string;
  TDynCardinalArray = array of Cardinal;

var
  Logger: TMyLameLogger;
  g_szCurDir: string;
  g_szDumpsDir: string;
  g_szPathSep: string;

  g_szBrushType: string;
  g_szBrushGenType: string;
  g_szGeometrySource: string;
  g_bConvertToED: Boolean;
  g_bDumpNodes: Boolean;
  //g_bIgnoreObjects: Boolean;
  g_szLightAnimsJob: string;
  g_bDebugProps: Boolean;
  g_nGlobalLogIndex: Integer = LOG_GENERIC;

  g_bLMFramesToSeparateTGA: Boolean = False;
  g_bAdditionalTexturesLTA: Boolean = False;
  //g_bLightMapTexturesLTA: Boolean = False;


function DumpExceptionCallStack(E: Exception): string;
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
procedure SimpleBlt16(Dest: TDynWordArray; Source: TDynWordArray; nDestWidth: Word; nSourceWidth: Word; nSourceHeight: Word; nX: Word; nY: Word);
procedure SimpleReverseBlt32(Source: TDynCardinalArray; Dest: TDynCardinalArray; nSourceWidth: Word; nDestWidth: Word; nDestHeight: Word; nX: Word; nY: Word);
function CompareDynArrays(anLeft: TDynByteArray; anRight: TDynByteArray; nLength: Cardinal): Integer;
function DynArray_Sha1Hash(Source: TDynByteArray; nLength: Cardinal): string;

implementation

function DumpExceptionCallStack(E: Exception): string;
var
  I: Integer;
  Frames: PPointer;
begin
  Result := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Result := Result + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Result := Result + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Result := Result + LineEnding + BackTraceStrFunc(Frames[I]);
end;

procedure WLogReal(S: string; F: LTFloat);
begin
  Logger.WLog(g_nGlobalLogIndex, etInfo, '| ' + S + ' = ' + FormatFloat('0.000000', F));
end;

procedure WLogVec(S: string; V: PLTVector);
begin
  Logger.WLog(g_nGlobalLogIndex, etInfo, '| ' + S + ' = ' + LTVectorToStrC(V));
end;

procedure WLogStr(S: string);
begin
  Logger.WLog(g_nGlobalLogIndex, etInfo, S);
end;

procedure WLogStrWarn(S: string);
begin
  Logger.WLog(g_nGlobalLogIndex, etWarning, S);
end;

procedure WLogInt(S: string; N: cardinal);
begin
  Logger.WLog(g_nGlobalLogIndex, etInfo, '| ' + S + ' = ' + IntToStr(N));
end;

procedure WLogAddr(S: string; N: cardinal);
begin
  Logger.WLog(g_nGlobalLogIndex, etInfo, '| ' + S + ' = $' + IntToHex(N, 8));
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
    r5, g6, b5: Byte;
    nColorWord, nTemp: Word;
begin
  anHeader[2] := 2; // TrueColor
  PWord(@anHeader[12])^ := nWidth; // 12 - 13
  PWord(@anHeader[14])^ := nHeight; // 14 - 15
  anHeader[16] := 8 * nChannels; // 32 bits
  if nChannels = 4 then
    anHeader[17] := 8 // Dunno
  else
    anHeader[17] := 0; // Dunno

  nSize := nWidth * nHeight * 2; //nChannels;

  MS := TMemoryStream.Create;
  MS.Write(anHeader[0], 18);

  //red8bit   = (red5bit << 3) | (red5bit >> 2);
  //green8bit = (green6bit << 2) | (green6bit >> 4);
  //blue8bit  = (blue5bit << 3) | (blue5bit >> 2);


  i := 0;
  while i < nSize do
  begin

    nColorWord := PWord(@Buffer[i])^;

    {$ASMMODE intel}
    asm
      mov ax, nColorWord
      shr ax, 11
      mov r5, al;
      mov ax, nColorWord
      shl ax, 5
      shr ax, 10
      mov g6, al;
      mov ax, nColorWord
      shl ax, 11
      shr ax, 11
      mov b5, al;
    end;

{red8bit   = (red5bit << 3) | (red5bit >> 2);
green8bit = (green6bit << 2) | (green6bit >> 4);
blue8bit  = (blue5bit << 3) | (blue5bit >> 2); }


    if bSwapRB then
    begin
      {MS.WriteByte(Buffer[i + 0]); // R
      MS.WriteByte(Buffer[i + 1]); // G
      MS.WriteByte(Buffer[i + 2]); // B   }
      MS.WriteByte((r5 shl 3) or (r5 shr 2));
      MS.WriteByte((g6 shl 2) or (g6 shr 4));
      MS.WriteByte((b5 shl 3) or (b5 shr 2));
    end
    else
    begin
      {MS.WriteByte(Buffer[i + 2]); // B
      MS.WriteByte(Buffer[i + 1]); // G
      MS.WriteByte(Buffer[i + 0]); // R}
      MS.WriteByte((b5 shl 3) or (b5 shr 2));
      MS.WriteByte((g6 shl 2) or (g6 shr 4));
      MS.WriteByte((r5 shl 3) or (r5 shr 2));
    end;

    if nChannels = 4 then
    begin
      if not bSolid then
        MS.WriteByte(Buffer[i + 3]) // A
      else
        MS.WriteByte($FF);
    end;

    //Inc(i, nChannels);
    Inc(i, 2);
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

procedure SimpleBlt16(Dest: TDynWordArray; Source: TDynWordArray; nDestWidth: Word; nSourceWidth: Word; nSourceHeight: Word; nX: Word; nY: Word);
var i: Word;
begin
  for i := 0 to nSourceHeight - 1 do
  begin
    Move(Source[i * nSourceWidth], Dest[(nY * nDestWidth + nX) + (nDestWidth * i)], nSourceWidth * 2);
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

function DynArray_Sha1Hash(Source: TDynByteArray; nLength: Cardinal): string;
begin
  if nLength > 0 then
    Result := SHA1Print(SHA1Buffer(Source[0], nLength))
  else
    Result := 'None';
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

