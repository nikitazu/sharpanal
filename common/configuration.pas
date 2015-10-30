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
    appDataDir: String;
    appDir: String;
    databasesDir: String;
    configPath: String;
  end;

implementation

// Windows
// LOCALAPPDATA=X:\Users\username\AppData\Local\
// X:\Users\username\AppData\Local\sharpanal\
// X:\Users\username\AppData\Local\sharpanal\config.ini

constructor TConfig.Create;
begin
  appDataDir := AppendPathDelim(GetEnvironmentVariable('LOCALAPPDATA'));
  appDir := appDataDir + AppendPathDelim('sharpanal');
  databasesDir := appDir + AppendPathDelim('db');
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
begin
  Result := projectName + '';
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

