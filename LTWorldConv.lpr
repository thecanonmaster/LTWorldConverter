program LTWorldConv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, math, MyLogger, ltworldreader, ltaworldexporter,
  globals, uvtoopq, edworldexporter;

const
  CLASSES_WITH_BRUSHES_FILENAME = 'classes_with_brushes.txt';
  CLASSES_WITH_BRUSHES_SKIP_LINE = '###';

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
var strFilename: string;
    strClassesWithBrushes: string;
    slClassesWithBrushes: TStringList;
    WorldReader: TLTWorldReader;
    LTAExporter: TLTAWorldExporter;
    EDExporter: TEDWorldExporter;
    szLogLevel: string;
    i: Integer;
begin
  Application.StopOnException := True;

  FormatSettings.DecimalSeparator := '.';
  strFilename := GetOptionValue('f', '');
  strClassesWithBrushes := GetOptionValue('c', '');
  if strClassesWithBrushes = '' then
    strClassesWithBrushes := CLASSES_WITH_BRUSHES_FILENAME;
  g_szBrushType := GetOptionValue('b', '');
  g_szBrushGenType := GetOptionValue('g', '');
  g_szGeometrySource := GetOptionValue('s', '');
  g_bConvertToED := HasOption('e', '');
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

  if strFilename = '' then
  begin
    WriteLn('LithTech Shogo World Converter ', LTWC_VERSION);
    WriteLn('Options:');
    WriteLn('Using: LTWorldConv.exe -f InputFile.dat -e OutputFile.lta -g poly');
    WriteLn('Options:');
    WriteLn('   -f: Input DAT world file. Version 56 is supported (Shogo).');
    WriteLn('   -e: Convert into ED format instead of LTA.');
    //WriteLn('   -b: Brush type (avp2 or nolf2). For AVP2 or NOLF2.');
    WriteLn('   -g: Brush generation type (simple or poly). For each worldmodel or for each poly.');
    WriteLn('   -n: Dump BSP nodes.');
    WriteLn('   -d: "Debug" mode, adds comments into LTA file.');
    WriteLn('   -s: Main geometry source (physics or vis).');
    //WriteLn('   -i: Do not include objects into LTA world file.');
    WriteLn('   -l: Light animations job (read or save).');
    WriteLn('   -o: Log level 1 - 3 (INFO, WARN, ERROR).');
  end;

  //if (g_szBrushType <> 'nolf2') and (g_szBrushType <> 'avp2') then
    g_szBrushType := 'avp2';
  if (g_szBrushGenType <> 'simple') and (g_szBrushGenType <> 'poly') then
    g_szBrushGenType := 'poly';
  if (g_szGeometrySource <> 'physics') and (g_szGeometrySource <> 'vis') then
    g_szGeometrySource := 'physics';

  if FileExists(strFilename) then
  begin
    try
      WorldReader := TLTWorldReader.Create(strFilename);
      WorldReader.ReadWorld;

      if not g_bConvertToED then
      begin
        LTAExporter := TLTAWorldExporter.Create(WorldReader);
        LTAExporter.ExportText(strFilename + '.lta');
        LTAExporter.Free;
      end
      else
      begin
        slClassesWithBrushes := TStringList.Create;
        if FileExists(strClassesWithBrushes) then
        begin
          slClassesWithBrushes.LoadFromFile(strClassesWithBrushes);
          i := 0;
          while i < slClassesWithBrushes.Count do
          begin
            if slClassesWithBrushes.Strings[i].StartsWith(CLASSES_WITH_BRUSHES_SKIP_LINE) then
              slClassesWithBrushes.Delete(i)
            else
              Inc(i, 1);
          end;
        end;
        EDExporter := TEDWorldExporter.Create(WorldReader, slClassesWithBrushes);
        EDExporter.ExportFile(strFilename + '.ed');
        EDExporter.Free;

        slClassesWithBrushes.Free;
      end;

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
    if strFilename <> '' then
      WLogStrWarn('World not found: ' + strFilename);
  end;
  Terminate;
end;

constructor TLTWorldConv.Create(TheOwner: TComponent);
var slLoggerNames: TStringList;
    strLogsDir: string;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

  inherited Create(TheOwner);
  g_szCurDir := ExtractFileDir(Application.ExeName);
  {$IFDEF LINUX}
  g_szPathSep := '/';
  {$ELSE}
  g_szPathSep := '\';
  {$ENDIF}
  g_szDumpsDir := g_szCurDir + g_szPathSep + 'dumps';
  strLogsDir := g_szCurDir + g_szPathSep + 'logs';

  CreateDir(g_szDumpsDir);
  CreateDir(strLogsDir);

  slLoggerNames := TStringList.Create;
  slLoggerNames.Add('generic');
  slLoggerNames.Add('header');
  slLoggerNames.Add('world_tree');
  slLoggerNames.Add('world_models');
  slLoggerNames.Add('objects');
  slLoggerNames.Add('render_data');
  Logger := TMyLameLogger.Create(strLogsDir + g_szPathSep + 'log', slLoggerNames);
  slLoggerNames.Free;
end;

destructor TLTWorldConv.Destroy;
begin
  inherited Destroy;
  // cleanup
  Logger.Free;
end;

begin
  Application:=TLTWorldConv.Create(nil);
  Application.Run;
  Application.Free;
end.

