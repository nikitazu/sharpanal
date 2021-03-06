unit ConfigCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, FileUtil,
  AbstractCommand;

type
  TConfigCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
    protected
      procedure OnRun; override;
      function ShortOptions: String; override;
      function LongOptions: String; override;
  end;

implementation

class function TConfigCommand.CommandName: ShortString;
begin
  Result := 'config';
end;

procedure TConfigCommand.OnRun;
var
  key: String;
  value: String;
begin
  key := _app.GetOptionValue('k','key');

  if not DirectoryExistsUTF8(_config.GetConfigPath)
  then CreateDirUTF8(_config.GetConfigPath);
  if not DirectoryExistsUTF8(_config.GetDatabasesPath)
  then CreateDirUTF8(_config.GetDatabasesPath);
  begin
    Log('start');
    if not IsEmptyStr(key, [#9]) then begin
      value := _config.GetValue('system', key, '');
      WriteLn(key + '=' + value);
    end
    else Error('no key asked');
  end;
end;

function TConfigCommand.ShortOptions: String;
begin
  Result := 'k:s:';
end;

function TConfigCommand.LongOptions: String;
begin
  Result := 'key: set:';
end;

end.

