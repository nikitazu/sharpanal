unit LinkCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, INIFiles, FileUtil,
  Configuration, AbstractCommand;

type
  TLinkCommand = class(TAbstractCommand)
    public
      procedure Run; override;
  end;

implementation

procedure TLinkCommand.Run;
var
  projectName: String;
  pathToSolution: String;
  error: String;
  config: TConfig;
  configFile: TINIFile;
begin
  inherited;
  error := _app.CheckOptions('hn:p:','help name: path:');
  if error <> '' then begin
     _app.ShowException(Exception.Create(error));
     _app.Terminate;
     Exit;
  end;

  projectName := _app.GetOptionValue('n','name');
  pathToSolution := _app.GetOptionValue('p','path');

  writeln('link start: ' + pathToSolution);
  config := TConfig.Create;
  if IsEmptyStr(Trim(projectName), [#9]) then
  begin
    writeln('link error: missing argument - name');
    writeln('link hint: name should be the same as in init command');
  end
  else if not DirectoryExistsUTF8(config.GetDatabasePath(projectName)) then
  begin
    writeln('link error: database not found - ' + projectName);
    writeln('link hint: name should be the same as in init command');
  end
  else if not FileExistsUTF8(pathToSolution) then
  begin
    writeln('link error: file not found - ' + pathToSolution);
    writeln('link hint: path should lead to Visual Studio solution file');
  end
  else if not DirectoryExistsUTF8(config.GetDatabasePath(projectName)) then
  begin
    writeln('link error: analizer not initialized - ' + projectName);
    writeln('link hint: use init ' + projectName);
  end
  else
  begin
    try
      configFile := config.GetOrCreateConfigFile;
      configFile.WriteString('links', projectName, pathToSolution);
    finally
      configFile.Free;
    end;
    writeln('link done');
  end;
end;

end.

