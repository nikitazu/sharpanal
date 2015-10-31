unit LinkCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, INIFiles, FileUtil,
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

  if AssertNotEmpty(projectName, 'name', 'name should be the same as in init command')
  and AssertNotEmpty(pathToSolution, 'path', 'path should lead to Visual Studio solution file')
  and AssertDirExists(config.GetDatabasePath(projectName), 'name should be the same as in init command')
  and AssertFileExists(pathToSolution, 'path should lead to Visual Studio solution file')
  then begin
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

