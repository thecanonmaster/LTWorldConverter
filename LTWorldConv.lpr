program LTWorldConv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, MyCrossPlatform,
  MyLogger, ltworldreader, ltaworldexporter, globals;

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
    //LTAFilename: string;
    WorldReader: TLTWorldReader;
    LTAExporter: TLTAWorldExporter;
    szLogLevel: string;
begin
  CreateDir(CPData.Dir + CPData.Sep + 'dumps');

  FormatSettings.DecimalSeparator := '.';
  Filename := GetOptionValue('f', '');
  //LTAFilename := GetOptionValue('e', '');
  g_szBrushType := GetOptionValue('b', '');
  g_szBrushGenType := GetOptionValue('g', '');
  g_szGeometrySource := GetOptionValue('s', '');
  g_bDumpNodes := HasOption('d', '');
  g_bIgnoreObjects := HasOption('i', '');
  g_bReadLightAnims := HasOption('l', '');

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
    WriteLn('   -d: Dump BSP nodes.');
    WriteLn('   -s: Main geometry source (physics, vis or both).');
    //WriteLn('   -i: Do not include objects into LTA world file.');
    WriteLn('   -l: Read lightanims and save to disk.');
    WriteLn('   -o: Log level 0 - 3 (INFO - DEBUG).');
  end;
  //if not CPData.FileExists(LTAFilename) then LTAFilename := 'world.lta';
  if (g_szBrushType <> 'nolf2') and (g_szBrushType <> 'avp2') then g_szBrushType := 'avp2';

  if (g_szBrushGenType <> 'simple') and (g_szBrushGenType <> 'poly') then g_szBrushGenType := 'poly';

  if (g_szGeometrySource <> 'physics') and (g_szGeometrySource <> 'vis') and (g_szGeometrySource <> 'both')
    then g_szGeometrySource := 'physics';

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
