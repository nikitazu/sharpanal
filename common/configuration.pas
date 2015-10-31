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
    _appDir: String;
    _databasesDir: String;
    _configPath: String;
  end;

implementation

constructor TConfig.Create;
begin
  _appDir := GetAppConfigDirUTF8(false, true);
  _databasesDir := AppendPathDelim(_appDir) + AppendPathDelim('db');
  _configPath := _appDir + 'config.ini';
end;

function TConfig.GetConfigPath: String;
begin
  Result := _configPath;
end;

function TConfig.GetDatabasesPath: String;
begin
  Result := _databasesDir;
end;

function TConfig.GetDatabasePath(projectName: String): String;
begin
  Result := _databasesDir + projectName;
end;

function TConfig.GetSolutionPath(projectName: String): String;
var
  config: TIniFile;
begin
  config := TIniFile.Create(_configPath);
  try
    Result := config.ReadString('links', projectName, '');
  finally
    config.Free;
  end;
end;

function TConfig.GetOrCreateConfigFile: TINIFile;
begin
  if not FileExistsUTF8(_configPath) then
  begin
    Result := TIniFile.Create(_configPath);
    Result.WriteString('system', 'foo', 'bar');
  end
  else Result := TIniFile.Create(_configPath);
end;

end.

