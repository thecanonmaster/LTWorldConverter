unit globals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MyLogger, MyCrossPlatform, ltworldtypes;

var
  Logger: TMyLameLogger;
  CPData: TMyCrossPlatform;

procedure WLogReal(S: string; F: LTFloat);
procedure WLogVec(S: string; V: PLTVector);
procedure WLogStr(S: string);
procedure WLogInt(S: string; N: cardinal);
procedure WLogAddr(S: string; N: cardinal);

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

procedure WLogInt(S: string; N: cardinal);
begin
  Logger.WLog(LM_INFO, '| ' + S + ' = ' + IntToStr(N));
end;

procedure WLogAddr(S: string; N: cardinal);
begin
  Logger.WLog(LM_INFO, '| ' + S + ' = $' + IntToHex(N, 8));
end;

end.

