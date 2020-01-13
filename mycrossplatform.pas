unit MyCrossPlatform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dos;

type
  TMyCrossPlatform = class(TObject)
  private
    FCurDir: string;
    FSeparator: Char;
  public
    property Dir: string read FCurDir write FCurDir;
    property Sep: Char read FSeparator write FSeparator;
    constructor Create;
    destructor Destroy; override;
    function FileExists(Filename: string): boolean;
  end;

implementation

function TMyCrossPlatform.FileExists(Filename: string): boolean;
var
  sDir: SearchRec;
begin
  Result := False;
  FindFirst(Filename, archive, sDir{%H-});
  while (DosError = 0) do
  begin
    FindNext(sDir);
    Result := True;
    Break;
  end;
  FindClose(sDir);
end;

constructor TMyCrossPlatform.Create;
begin
  // get current directory
  getdir(0, FCurDir);
  // set separator
  {$IFDEF LINUX}
  FSeparator := '/';
  {$ELSE}
  FSeparator := '\';
  {$ENDIF}
end;

destructor TMyCrossPlatform.Destroy;
begin
  {$IFDEF LINUX}

  {$ELSE}

  {$ENDIF}
end;

end.

