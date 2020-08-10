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
      Logger.RootLevel := TEventType(StrToInt(szLogLevel));
    except on E: EConvertError do
      Logger.RootLevel := etWarning;
    end;
  end
  else
  begin
    Logger.RootLevel := etWarning;
  end;

  if Filename = '' then
  begin
    WriteLn('LithTech Talon World Converter ', LTWC_VERSION);
    WriteLn('Options:');
    WriteLn('Using: LTWorldConv.exe -f InputFile.dat -e OutputFile.lta -g poly');
    WriteLn('Options:');
    WriteLn('   -f: Input DAT world file. Version 70 is supported (AVP 2).');
    WriteLn('   -e: Output LTA world file. Can be imported into AVP2 DEdit.');
    //WriteLn('   -b: Brush type (avp2 or nolf2). For AVP2 or NOLF2.');
    WriteLn('   -g: Brush generation type (simple or poly). For each worldmodel or for each poly.');
    WriteLn('   -n: Dump BSP nodes.');
    WriteLn('   -d: "Debug" mode, adds comments into LTA file.');
    WriteLn('   -s: Main geometry source (physics or vis).');
    //WriteLn('   -i: Do not include objects into LTA world file.');
    WriteLn('   -l: Light animations job (read, save or load).');
    WriteLn('   -o: Log level 1 - 3 (INFO, WARN, ERROR).');
  end;

  if (g_szBrushType <> 'nolf2') and (g_szBrushType <> 'avp2') then
    g_szBrushType := 'avp2';
  if (g_szBrushGenType <> 'simple') and (g_szBrushGenType <> 'poly') then
    g_szBrushGenType := 'poly';
  if (g_szGeometrySource <> 'physics') and (g_szGeometrySource <> 'vis') then
    g_szGeometrySource := 'physics';

  if (CPData.FileExists(Filename)) then
  begin
    try
      WorldReader := TLTWorldReader.Create(Filename);
      WorldReader.ReadWorld;
      LTAExporter := TLTAWorldExporter.Create(WorldReader);
      LTAExporter.ExportText(Filename + '.lta');
      LTAExporter.Free;
      WorldReader.Free;
    except
      on E: Exception do
      begin
        Logger.WLog(0, etError, DumpExceptionCallStack(E));
        Logger.Flush;
      end;
    end;
  end
  else
  begin
    if Filename <> '' then
      WLogStrWarn('World not found: ' + Filename);
  end;
  Terminate;
end;

constructor TLTWorldConv.Create(TheOwner: TComponent);
var slLoggerNames: TStringList;
begin
  inherited Create(TheOwner);
  // init
  CPData := TMyCrossPlatform.Create;

  CreateDir(CPData.Dir + CPData.Sep + 'dumps');
  CreateDir(CPData.Dir + CPData.Sep + 'logs');

  slLoggerNames := TStringList.Create;
  slLoggerNames.Add('generic');
  slLoggerNames.Add('header');
  slLoggerNames.Add('world_tree');
  slLoggerNames.Add('world_models');
  slLoggerNames.Add('objects');
  slLoggerNames.Add('render_data');
  Logger := TMyLameLogger.Create(CPData.Dir + CPData.Sep + 'logs' + CPData.Sep + 'log', slLoggerNames);
  slLoggerNames.Free;
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

