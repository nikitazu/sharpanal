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
      procedure Run; override;
  end;

implementation


class function TLinkCommand.CommandName: ShortString;
begin
  Result := 'link';
end;


procedure TLinkCommand.Run;
var
  projectName: String;
  pathToSolution: String;
  argsError: String;
  config: TConfig;
  configFile: TINIFile;
begin
  inherited;
  argsError := _app.CheckOptions('hvn:p:','help verbose name: path:');
  if argsError <> '' then begin
     _app.ShowException(Exception.Create(argsError));
     _app.Terminate;
     Exit;
  end;

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
    Log('done');
  end;
end;

end.

