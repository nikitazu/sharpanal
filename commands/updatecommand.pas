unit UpdateCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil,
  AbstractCommand,
  SolutionModel, ProjectModel,
  DbfIndexedStorage;

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
  storage: TDbfIndexedStorage;
  solution: TSolutionModel;
  solutionId: Integer;
  projectRelativePath: String;
  projectFullPath: String;
  project: TProjectModel;
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
        solution := TSolutionModel.Create(self);
        solution.Load(pathToSolution);
        solutionId := storage.AppendSolution(solution.Title);
        for projectRelativePath in solution.Projects do begin
          projectFullPath := AppendPathDelim(ExtractFileDir(pathToSolution)) +
            projectRelativePath;
          Log('load project: ' + projectFullPath);
          project := TProjectModel.Create(solution);
          project.Load(projectFullPath);
          storage.AppendProject(project.Title, projectRelativePath, solutionId);
        end;
      except
        on e : Exception do begin
          Error(Format('%s - %s', [e.ClassName, e.Message]));
        end;
      end;
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

