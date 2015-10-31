unit ConfigCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, FileUtil, INIFiles,
  Configuration, AbstractCommand;

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
  config: TConfig;
  configFile: TINIFile;
  value: String;
begin
  key := _app.GetOptionValue('k','key');

  Log('start');
  config := TConfig.Create;
  if not DirectoryExistsUTF8(config.GetConfigPath)
  then CreateDirUTF8(config.GetConfigPath);
  if not DirectoryExistsUTF8(config.GetDatabasesPath)
  then CreateDirUTF8(config.GetDatabasesPath);
  try
    configFile := config.GetOrCreateConfigFile;
    if not IsEmptyStr(key, [#9]) then
    begin
      value := configFile.ReadString('system', key, '');
      WriteLn(key + '=' + value);
    end
    else Error('no key asked');
  finally
    configFile.Free;
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

