unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, INIFiles;

type
  TConfig = class(TObject)
  public
    constructor Create;
    destructor Destroy; override;

    function GetConfigPath: String;
    function GetDatabasesPath: String;
    function GetDatabasePath(projectName: String): String;

    function GetSolutionPath(projectName: String): String;
    procedure SetSolutionPath(projectName: String; path: String);

    function GetValue(section: String; key: String; empty: String): String;
    procedure SetValue(section: String; key: String; value: String);

  private
    function GetOrCreateConfigFile: TINIFile;
  private
    _appDir: String;
    _databasesDir: String;
    _configPath: String;
    _config: TIniFile;
  end;

implementation

constructor TConfig.Create;
begin
  _appDir := GetAppConfigDirUTF8(false, true);
  _databasesDir := AppendPathDelim(_appDir) + AppendPathDelim('db');
  _configPath := _appDir + 'config.ini';
  _config := GetOrCreateConfigFile;
end;

destructor TConfig.Destroy;
begin
  FreeAndNil(_config);
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
begin
  Result := _config.ReadString('links', projectName, '');
end;

procedure TConfig.SetSolutionPath(projectName: String; path: String);
begin
  _config.WriteString('links', projectName, path);
end;

function TConfig.GetValue(section: String; key: String; empty: String): String;
begin
  Result := _config.ReadString(section, key, empty);
end;

procedure TConfig.SetValue(section: String; key: String; value: String);
begin
  _config.WriteString(section, key, value);
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

