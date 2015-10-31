unit UpdateCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  AbstractCommand;

type
  TUpdateCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
    protected
      procedure OnRun; override;
      function ShortOptions: String; override;
      function LongOptions: String; override;
  end;

implementation

class function TUpdateCommand.CommandName: ShortString;
begin
  Result := 'update';
end;

procedure TUpdateCommand.OnRun;
var
  projectName: String;
  databasePath: String;
  pathToSolution: String;
begin
  projectName := _app.GetOptionValue('n','name');

  if AssertNotEmpty(projectName, 'name', 'name should be the same as in init command')
  then begin
    Log('start: ' + projectName);
    databasePath := _config.GetDatabasePath(projectName);
    pathToSolution := _config.GetSolutionPath(projectName);
    if AssertNotEmpty(pathToSolution, 'path', 'path should lead to Visual Studio solution file')
    and AssertDirExists(databasePath, 'name should be the same as in init command')
    and AssertFileExists(pathToSolution, 'path should lead to Visual Studio solution file')
    then begin
      Log('analizing ' + pathToSolution);
      WriteLn('TODO');
    end;
  end;
end;

function TUpdateCommand.ShortOptions: String;
begin
  Result := 'n:';
end;

function TUpdateCommand.LongOptions: String;
begin
  Result := 'name:';
end;

end.

