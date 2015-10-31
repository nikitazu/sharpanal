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
  end;

implementation

class function TConfigCommand.CommandName: ShortString;
begin
  Result := 'config';
end;

procedure TConfigCommand.OnRun;
var
  key: String;
  argsError: String;
  config: TConfig;
  configFile: TINIFile;
  value: String;
begin
  argsError := _app.CheckOptions('hvk:s:','help verbose key: set:');
  if argsError <> '' then begin
     _app.ShowException(Exception.Create(argsError));
     _app.Terminate;
     Exit;
  end;

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

end.

