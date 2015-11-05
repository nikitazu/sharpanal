unit FindCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  AbstractCommand,
  DbfIndexedStorage,
  Entities;

type
  TFindCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
      procedure OnFindSolution(s: SolutionEntity);
      procedure OnFindProject(p: ProjectEntity);
      procedure OnFindFile(f: FileEntity);
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
  query: String;
  queryType: String;
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
      Log('finding ' + pathToSolution);
      try
        storage := TDbfIndexedStorage.Create(self);
        storage.IsDebug := _app.HasOption('v','verbose');
        storage.DatabasePath := databasePath;
        query := Trim(_app.GetOptionValue('q','query'));
        queryType := Trim(_app.GetOptionValue('t','type'));

        if query = '' then begin
          if (queryType = '') or (queryType = 'solution')
          then storage.SolutionsDo(@OnFindSolution);
          if (queryType = '') or (queryType = 'project')
          then storage.ProjectsDo(@OnFindProject);
          if (queryType = '') or (queryType = 'file')
          then storage.FilesDo(@OnFindFile);
        end else begin
          // todo search in files
          storage.FindFiles(@OnFindFile, query);
        end;
      except
        on e : Exception do begin
          Error(Format('%s - %s', [e.ClassName, e.Message]));
        end;
      end;
    end;
  end;
end;

procedure TFindCommand.OnFindSolution(s: SolutionEntity);
begin
  WriteLn(Format('solution #%d %s', [s.Id, s.Title]));
end;

procedure TFindCommand.OnFindProject(p: ProjectEntity);
begin
  WriteLn(Format('project #%d %s - %s (s#%d)',
    [p.Id, p.Title, p.Path, p.SolutionId]));
end;

procedure TFindCommand.OnFindFile(f: FileEntity);
begin
  WriteLn(Format('file #%d %s - %s (s#%d p#%d)',
    [f.Id, f.Title, f.Path, f.SolutionId, f.ProjectId]));
end;

function TFindCommand.ShortOptions: String;
begin
  Result := 'n:q:t:';
end;

function TFindCommand.LongOptions: String;
begin
  Result := 'name: query: type:';
end;

end.

