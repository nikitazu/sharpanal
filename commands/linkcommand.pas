unit LinkCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, INIFiles, FileUtil,
  Configuration, AbstractCommand;

type
  TLinkCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
    protected
      procedure OnRun; override;
      function ShortOptions: String; override;
      function LongOptions: String; override;
  end;

implementation

class function TLinkCommand.CommandName: ShortString;
begin
  Result := 'link';
end;

procedure TLinkCommand.OnRun;
var
  projectName: String;
  pathToSolution: String;
  config: TConfig;
  configFile: TINIFile;
begin
  projectName := _app.GetOptionValue('n','name');
  pathToSolution := _app.GetOptionValue('p','path');

  Log('start: ' + pathToSolution);
  config := TConfig.Create;
  if IsEmptyStr(Trim(projectName), [#9]) then
  begin
    Error('missing argument - name');
    Log('hint: name should be the same as in init command');
  end
  else if not DirectoryExistsUTF8(config.GetDatabasePath(projectName)) then
  begin
    Error('database not found - ' + projectName);
    Log('hint: name should be the same as in init command');
  end
  else if IsEmptyStr(Trim(pathToSolution), [#9]) then
  begin
    Error('missing argument - path');
    Log('hint: path should lead to Visual Studio solution file');
  end
  else if not FileExistsUTF8(pathToSolution) then
  begin
    Error('file not found - ' + pathToSolution);
    Log('hint: path should lead to Visual Studio solution file');
  end
  else if not DirectoryExistsUTF8(config.GetDatabasePath(projectName)) then
  begin
    Error('analizer not initialized - ' + projectName);
    Log('hint: use init ' + projectName);
  end
  else
  begin
    try
      configFile := config.GetOrCreateConfigFile;
      configFile.WriteString('links', projectName, pathToSolution);
    finally
      configFile.Free;
    end;
  end;
end;

function TLinkCommand.ShortOptions: String;
begin
  Result := 'n:p:';
end;

function TLinkCommand.LongOptions: String;
begin
  Result := 'name: path:';
end;

end.

