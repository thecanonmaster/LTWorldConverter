unit MyLogger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog;

type
  TMyLameLogger = class(TObject)
  private
  protected
    m_pLogger2: TEventLog;
  public
    procedure WLog(Mode: integer; Msg: string);
    procedure WLogStrings(Mode: integer; szIdentifier: string; slList: TStringList);
    procedure WLogStringsEx(Mode: integer; szIdentifier: string; aslArgs: array of TStringList);
    constructor Create(szFileName: string); virtual;
    destructor Destroy; override;
  end;

const
  LM_INFO = 0;
  LM_WARN = 1;
  LM_ERROR = 2;
  LM_DEBUG = 3;
  STRSEP = '------------------------------------------------------------------------------------';

implementation

constructor TMyLameLogger.Create(szFileName: string);
begin
  m_pLogger2 := TEventLog.Create(nil);
  m_pLogger2.FileName := szFileName;
  m_pLogger2.LogType := ltFile;
  m_pLogger2.TimeStampFormat := 'hh:nn:ss.zzz';
  DeleteFile(szFileName);
  WLog(LM_INFO, 'Program started...');
end;

destructor TMyLameLogger.Destroy;
begin
  WLog(LM_INFO, 'Program stopped...');
  inherited;
end;

procedure TMyLameLogger.WLog(Mode: integer; Msg: string);
begin
  case Mode of
    LM_INFO: m_pLogger2.Info(Msg);
    LM_WARN:
      begin
        m_pLogger2.Warning(Msg);
        WriteLn('[WARNING] ', Msg);
      end;
    LM_ERROR:
      begin
        m_pLogger2.Error(Msg);
        WriteLn('[ERROR] ', Msg);
      end;
    LM_DEBUG: m_pLogger2.Debug(Msg);
  end;
end;

procedure TMyLameLogger.WLogStrings(Mode: integer; szIdentifier: string;
  slList: TStringList);
var
  i: integer;
  Level: TEventType;
begin
  case Mode of
    LM_INFO: Level := etInfo;
    LM_WARN: Level := etWarning;
    LM_ERROR: Level := etError;
    LM_DEBUG: Level := etDebug;
  end;
  for i := 0 to slList.Count - 1 do
  begin
    m_pLogger2.Log(Level, szIdentifier + ' | ' + slList[i]);
  end;
end;

procedure TMyLameLogger.WLogStringsEx(Mode: integer; szIdentifier: string; aslArgs: array of TStringList);
var
  i, j: integer;
  Level: TEventType;
  szBufferStr: string;
begin
  case Mode of
    LM_INFO: Level := etInfo;
    LM_WARN: Level := etWarning;
    LM_ERROR: Level := etError;
    LM_DEBUG: Level := etDebug;
  end;
  for i := 0 to aslArgs[0].Count - 1 do
  begin
    szBufferStr := '';
    szBufferStr := aslArgs[0].Strings[i];
    for j := 1 to Length(aslArgs) - 1 do
    begin
      szBufferStr := szBufferStr + ' / ' + aslArgs[j].Strings[i];
    end;
    m_pLogger2.Log(Level, szIdentifier + ' | ' + szBufferStr);
  end;
end;

end.
