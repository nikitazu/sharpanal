unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, INIFiles;

type
  TConfig = class(TObject)
  public
    constructor Create; overload;
    function GetConfigPath: String;
    function GetDatabasesPath: String;
    function GetDatabasePath(projectName: String): String;
    function GetSolutionPath(projectName: String): String;
    function GetOrCreateConfigFile: TINIFile;
  private
    appDir: String;
    databasesDir: String;
    configPath: String;
  end;

implementation

constructor TConfig.Create;
begin
  appDir := GetAppConfigDirUTF8(false, true);
  databasesDir := AppendPathDelim(appDir) + AppendPathDelim('db');
  configPath := appDir + 'config.ini';
end;

function TConfig.GetConfigPath: String;
begin
  Result := configPath;
end;

function TConfig.GetDatabasesPath: String;
begin
  Result := databasesDir;
end;

function TConfig.GetDatabasePath(projectName: String): String;
begin
  Result := databasesDir + projectName;
end;

function TConfig.GetSolutionPath(projectName: String): String;
var
  config: TIniFile;
begin
  config := TIniFile.Create(configPath);
  try
    Result := config.ReadString('links', projectName, '');
  finally
    config.Free;
  end;
end;

function TConfig.GetOrCreateConfigFile: TINIFile;
begin
  if not FileExistsUTF8(configPath) then
  begin
    Result := TIniFile.Create(configPath);
    Result.WriteString('system', 'foo', 'bar');
  end
  else Result := TIniFile.Create(configPath);
end;

end.

