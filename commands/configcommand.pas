unit ConfigCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, FileUtil, INIFiles;

procedure Run(key: String);
function GetOrCreateConfigFile(configPath: String): TINIFile;

implementation

// Windows
// LOCALAPPDATA=X:\Users\username\AppData\Local\
// X:\Users\username\AppData\Local\sharpanal\
// X:\Users\username\AppData\Local\sharpanal\config.ini

procedure Run(key: String);
var
  allConfigs: String;
  configDir: String;
  configPath: String;
  config: TINIFile;
  value: String;
begin
  WriteLn('config start');
  allConfigs := AppendPathDelim(GetEnvironmentVariable('LOCALAPPDATA'));
  configDir := allConfigs + 'sharpanal\';
  configPath := configDir + 'config.ini';
  if not DirectoryExists(configDir) then CreateDir(configDir);
  config := GetOrCreateConfigFile(configPath);
  if not IsEmptyStr(key, [#9]) then
  begin
    value := config.ReadString('system', key, '');
    WriteLn('config: ' + key + '=' + value);
  end
  else WriteLn('config: no key asked');
end;

function GetOrCreateConfigFile(configPath: String): TINIFile;
begin
  if not FileExists(configPath) then
  begin
    Result := TIniFile.Create(configPath);
    Result.WriteString('system', 'foo', 'bar');
  end
  else Result := TIniFile.Create(configPath);
end;

end.

