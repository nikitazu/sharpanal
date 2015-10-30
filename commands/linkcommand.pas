unit LinkCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, INIFiles, FileUtil,
  Configuration;

procedure Run(projectName: String; pathToSolution: String);

implementation

procedure Run(projectName: String; pathToSolution: String);
var
  config: TConfig;
  configFile: TINIFile;
begin
  writeln('link start: ' + pathToSolution);
  config := TConfig.Create;
  if IsEmptyStr(projectName, [#9]) then
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

