unit FindCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  AbstractCommand,
  DbfIndexedStorage;

type
  TFindCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
      procedure OnFindSolution(args: Array of const);
      procedure OnFindProject(args: Array of const);
      procedure OnFindFile(args: Array of const);
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

procedure TFindCommand.OnFindSolution(args: Array of const);
var
  solutionId: Integer;
  title: String;
begin
  solutionId := args[0].VInteger;
  title := AnsiString(args[1].VAnsiString);
  WriteLn(Format('solution #%d %s', [solutionId, title]));
end;

procedure TFindCommand.OnFindProject(args: Array of const);
var
  projectId: Integer;
  title: String;
  path: String;
  solutionId: Integer;
begin
  projectId := args[0].VInteger;
  title := AnsiString(args[1].VAnsiString);
  path := AnsiString(args[2].VAnsiString);
  solutionId := args[3].VInteger;
  WriteLn(Format('project #%d %s - %s (s#%d)', [projectId, title, path, solutionId]));
end;

procedure TFindCommand.OnFindFile(args: Array of const);
var
  fileId: Integer;
  title: String;
  path: String;
  solutionId: Integer;
  projectId: Integer;
begin
  fileId := args[0].VInteger;
  title := AnsiString(args[1].VAnsiString);
  path := AnsiString(args[2].VAnsiString);
  solutionId := args[3].VInteger;
  projectId := args[4].VInteger;
  WriteLn(Format('file #%d %s - %s (s#%d p#%d)', [fileId, title, path, solutionId, projectId]));
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

