unit MyLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  LOG_GENERIC = 0;
  LOG_HEADER = 1;
  LOG_WORLD_TREE = 2;
  LOG_WORLD_MODELS = 3;
  LOG_OBJECTS = 4;
  LOG_RENDER_DATA = 5;

type

  { TMyLameLogger }

  TMyLameLogger = class(TObject)
  private
  protected
    m_eRootLevel: TEventType;
    m_pLoggers: array of TStringList;
    m_pFullnames: array of string;
  public
    property RootLevel: TEventType read m_eRootLevel write m_eRootLevel;
    function CheckRootLevel(Mode: TEventType): Boolean;
    procedure WLog(Index: Integer; Mode: TEventType; Msg: string);
    procedure WLogStrings(Index: Integer; Mode: TEventType; szIdentifier: string;
      slList: TStringList);
    procedure WLogStringsEx(Index: Integer; Mode: TEventType; szIdentifier: string;
      aslArgs: array of TStringList);
    procedure Flush;
    constructor Create(szFilename: string; slLoggerNames: TStringList); virtual;
    destructor Destroy; override;
  end;

const
  STRSEP = '------------------------------------------------------------------------------------';

implementation

constructor TMyLameLogger.Create(szFilename: string; slLoggerNames: TStringList);
var pLogger: TStringList;
    i: Integer;
begin
  SetLength(m_pLoggers, slLoggerNames.Count);
  SetLength(m_pFullnames, slLoggerNames.Count);
  for i := 0 to slLoggerNames.Count - 1 do
  begin
    {pLogger := TEventLog.Create(nil);
    pLogger.FileName := szFilename + '_' + slLoggerNames.Strings[i] + '.txt';
    pLogger.LogType := ltFile;
    pLogger.TimeStampFormat := 'hh:nn:ss.zzz'; }

    pLogger := TStringList.Create;
    m_pFullnames[i] := szFilename + '_' + slLoggerNames.Strings[i] + '.txt';
    m_pLoggers[i] := pLogger;
    DeleteFile(m_pFullnames[i]);
  end;

  m_eRootLevel := etInfo;
  WLog(LOG_GENERIC, etInfo, 'Program started...');
end;

destructor TMyLameLogger.Destroy;
var i: Integer;
begin
  m_eRootLevel := etInfo;
  WLog(LOG_GENERIC, etInfo, 'Program stopped...');
  Flush;
  for i := 0 to Length(m_pLoggers) - 1 do
    m_pLoggers[i].Free;
  SetLength(m_pLoggers, 0);
  SetLength(m_pFullnames, 0);
  inherited;
end;

function TMyLameLogger.CheckRootLevel(Mode: TEventType): Boolean;
begin
  Result := (m_eRootLevel <= Mode);
end;

procedure TMyLameLogger.WLog(Index: Integer; Mode: TEventType; Msg: string);
begin
  if not CheckRootLevel(Mode) then Exit;

  m_pLoggers[Index].Add(Msg);

  if Mode = etWarning then
    WriteLn('[WARNING] ', Msg)
  else if Mode = etError then
    WriteLn('[ERROR] ', Msg);
end;

procedure TMyLameLogger.WLogStrings(Index: Integer; Mode: TEventType; szIdentifier: string;
  slList: TStringList);
var
  i: Integer;
begin
  if not CheckRootLevel(Mode) then Exit;

  for i := 0 to slList.Count - 1 do
  begin
    m_pLoggers[Index].Add(szIdentifier + ' | ' + slList[i]);
  end;
end;

procedure TMyLameLogger.WLogStringsEx(Index: Integer; Mode: TEventType; szIdentifier: string; aslArgs: array of TStringList);
var
  i, j: Integer;
  szBufferStr: string;
begin
  if not CheckRootLevel(Mode) then Exit;

  for i := 0 to aslArgs[0].Count - 1 do
  begin
    szBufferStr := '';
    szBufferStr := aslArgs[0].Strings[i];
    for j := 1 to Length(aslArgs) - 1 do
    begin
      szBufferStr := szBufferStr + ' / ' + aslArgs[j].Strings[i];
    end;
    m_pLoggers[Index].Add(szIdentifier + ' | ' + szBufferStr);
  end;
end;

procedure TMyLameLogger.Flush;
var i: Integer;
begin
  for i := 0 to Length(m_pLoggers) - 1 do
  begin
    m_pLoggers[i].SaveToFile(m_pFullnames[i]);
  end;
end;

end.
