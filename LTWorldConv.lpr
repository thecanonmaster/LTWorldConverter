program LTWorldConv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, MyCrossPlatform,
  MyLogger, ltworldreader, ltaworldexporter, globals, uvtoopq;

type
  TLTWorldConv = class(TCustomApplication)
  private
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Application: TLTWorldConv;

procedure TLTWorldConv.DoRun;
var Filename: string;
    WorldReader: TLTWorldReader;
    LTAExporter: TLTAWorldExporter;
    szLogLevel: string;
begin
  Application.StopOnException := True;
  CreateDir(CPData.Dir + CPData.Sep + 'dumps');

  FormatSettings.DecimalSeparator := '.';
  Filename := GetOptionValue('f', '');
  g_szBrushType := GetOptionValue('b', '');
  g_szBrushGenType := GetOptionValue('g', '');
  g_szGeometrySource := GetOptionValue('s', '');
  g_bDumpNodes := HasOption('n', '');
  //g_bIgnoreObjects := HasOption('i', '');
  g_szLightAnimsJob := GetOptionValue('l', '');
  g_bDebugProps := HasOption('d', '');

  if HasOption('o', '') then
  begin
    szLogLevel := GetOptionValue('o', '');
    try
      Logger.RootLevel := StrToInt(szLogLevel);
    except on E: EConvertError do
      Logger.RootLevel := LM_WARN;
    end;
  end
  else
  begin
    Logger.RootLevel := LM_WARN;
  end;

  if Filename = '' then
  begin
    WriteLn('LithTech Talon World Converter ', LTWC_VERSION);
    WriteLn('Options:');
    WriteLn('Using: LTWorldConv.exe -f InputFile.dat -e OutputFile.lta -b avp2 -g poly -d');
    WriteLn('Options:');
    WriteLn('   -f: Input DAT world file. Version 70 is supported (AVP 2).');
    WriteLn('   -e: Output LTA world file. Can be read by AVP2 and NOLF2 DEdits.');
    WriteLn('   -b: Brush type (avp2 or nolf2). For AVP2 or NOLF2.');
    WriteLn('   -g: Brush generation type (simple or poly). For each worldmodel or for each poly.');
    WriteLn('   -n: Dump BSP nodes.');
    WriteLn('   -d: "Debug" mode, adds comments into LTA file.');
    WriteLn('   -s: Main geometry source (physics or vis).');
    //WriteLn('   -i: Do not include objects into LTA world file.');
    WriteLn('   -l: Light animations job (save or load).');
    WriteLn('   -o: Log level 0 - 3 (INFO - DEBUG).');
  end;

  if (g_szBrushType <> 'nolf2') and (g_szBrushType <> 'avp2') then g_szBrushType := 'avp2';
  if (g_szBrushGenType <> 'simple') and (g_szBrushGenType <> 'poly') then g_szBrushGenType := 'poly';
  if (g_szGeometrySource <> 'physics') and (g_szGeometrySource <> 'vis') then g_szGeometrySource := 'physics';

  if (CPData.FileExists(Filename)) then
  begin
    WorldReader := TLTWorldReader.Create(Filename);
    WorldReader.ReadWorld;
    LTAExporter := TLTAWorldExporter.Create(WorldReader);
    LTAExporter.ExportText(Filename + '.lta');
    LTAExporter.Free;
    WorldReader.Free;
  end
  else
  begin
    if Filename <> '' then Logger.WLog(LM_WARN, 'World not found: ' + Filename);
  end;
  Terminate;
end;

constructor TLTWorldConv.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  // init
  CPData := TMyCrossPlatform.Create;
  Logger := TMyLameLogger.Create(CPData.Dir + CPData.Sep + 'log.txt');
end;

destructor TLTWorldConv.Destroy;
begin
  inherited Destroy;
  // cleanup
  Logger.Free;
  CPData.Free;
end;

begin
  Application:=TLTWorldConv.Create(nil);
  Application.Run;
  Application.Free;
end.

