unit ConfigCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, FileUtil, INIFiles,
  Configuration;

procedure Run(key: String);

implementation

procedure Run(key: String);
var
  config: TConfig;
  configFile: TINIFile;
  value: String;
begin
  WriteLn('config start');
  config := TConfig.Create;
  if not DirectoryExistsUTF8(config.GetConfigPath)
  then CreateDir(config.GetConfigPath);
  if not DirectoryExistsUTF8(config.GetDatabasesPath)
  then CreateDir(config.GetDatabasesPath);
  try
    configFile := config.GetOrCreateConfigFile;
    if not IsEmptyStr(key, [#9]) then
    begin
      value := configFile.ReadString('system', key, '');
      WriteLn('config: ' + key + '=' + value);
    end
    else WriteLn('config: no key asked');
  finally
    configFile.Free;
  end;
end;

end.

