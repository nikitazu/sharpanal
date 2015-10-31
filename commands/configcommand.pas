unit ConfigCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, FileUtil, INIFiles,
  Configuration, AbstractCommand;

type
  TConfigCommand = class(TAbstractCommand)
    public
      procedure Run; override;
  end;

implementation

procedure TConfigCommand.Run;
var
  key: String;
  error: String;
  config: TConfig;
  configFile: TINIFile;
  value: String;
begin
  inherited;
  error := _app.CheckOptions('hk:v:','help key: value:');
  if error <> '' then begin
     _app.ShowException(Exception.Create(error));
     _app.Terminate;
     Exit;
  end;

  key := _app.GetOptionValue('k','key');

  WriteLn('config start');
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
      WriteLn('config: ' + key + '=' + value);
    end
    else WriteLn('config: no key asked');
  finally
    configFile.Free;
  end;
end;

end.

