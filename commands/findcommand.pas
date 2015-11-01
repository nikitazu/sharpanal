unit FindCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  AbstractCommand,
  SolutionModel, ProjectModel, FileModel,
  DbfIndexedStorage;

type
  TFindCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
    protected
      procedure OnRun; override;
      function ShortOptions: String; override;
      function LongOptions: String; override;
  end;

implementation

class function TFindCommand.CommandName: ShortString;
begin
  Result := 'find';
end;

procedure TFindCommand.OnRun;
var
  projectName: String;
  databasePath: String;
  pathToSolution: String;
  storage: TDbfIndexedStorage;
  solution: TSolutionModel;
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
      try
        storage := TDbfIndexedStorage.Create(self);
        storage.IsDebug := _app.HasOption('v','verbose');
        storage.DatabasePath := databasePath;
        storage.SolutionsDo;
      except
        on e : Exception do begin
          Error(Format('%s - %s', [e.ClassName, e.Message]));
        end;
      end;
    end;
  end;
end;

function TFindCommand.ShortOptions: String;
begin
  Result := 'n:q';
end;

function TFindCommand.LongOptions: String;
begin
  Result := 'name: query solutions projects files all';
end;

end.

