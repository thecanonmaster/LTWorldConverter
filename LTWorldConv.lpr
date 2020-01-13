program LTWorldConv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, MyCrossPlatform,
  MyLogger, ltworldreader, ltaworldexporter, globals;

const
  LTWC_VERSION = 'v0.11 alpha';

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
    szBrushType: string;
    szBrushGenType: string;
    bDumpNodes: Boolean;
    bIgnoreObjects: Boolean;
begin
  CreateDir(CPData.Dir + CPData.Sep + 'dumps');

  FormatSettings.DecimalSeparator := '.';
  Filename := GetOptionValue('f', '');
  //LTAFilename := GetOptionValue('e', '');
  szBrushType := GetOptionValue('b', '');
  szBrushGenType := GetOptionValue('g', '');
  bDumpNodes := HasOption('d', '');
  bIgnoreObjects := HasOption('i', '');

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
    WriteLn('   -d: Dump BSP structures.');
    //WriteLn('   -i: Do not include objects into LTA world file.');
  end;
  //if not CPData.FileExists(LTAFilename) then LTAFilename := 'world.lta';
  if (szBrushType <> 'nolf2') and (szBrushType <> 'avp2') then szBrushType := 'avp2';
  if (szBrushGenType <> 'simple') and (szBrushGenType <> 'poly') then szBrushGenType := 'poly';

  if (CPData.FileExists(Filename)) then
  begin
    WorldReader := TLTWorldReader.Create(Filename, bDumpNodes);
    WorldReader.ReadWorld;
    LTAExporter := TLTAWorldExporter.Create(WorldReader, szBrushType, szBrushGenType, bIgnoreObjects);
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

