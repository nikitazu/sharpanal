unit DbfIndexedStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil, LazLogger,
  dbf, db,
  FileHelpers,
  Entities;

type
  TSolutionCallback = procedure(s: SolutionEntity) of object;
  TProjectCallback = procedure(p: ProjectEntity) of object;
  TFileCallback = procedure(p: FileEntity) of object;

  TDbfIndexedStorage = Class(TComponent)
    private
      _isDebug: Boolean;
      _databasePath: String;
      _solutionsTable: TDbf;
      _projectsTable: TDbf;
      _filesTable: TDbf;
    private
      procedure CreateSolutionsTable;
      procedure CreateProjectsTable;
      procedure CreateFilesTable;
      procedure SetDatabasePath(path: String);
      procedure Log(message: String);
    public
      constructor Create(aOwner: TComponent); override;
      procedure CreateTables;

      function AppendSolution(title: String): Integer;

      function AppendProject(
        title: String;
        path: String;
        solutionId: Integer): Integer;

      function AppendFile(
        title: String;
        path: String;
        solutionId: Integer;
        projectId: Integer): Integer;

      procedure SolutionsDo(cb: TSolutionCallback);
      procedure ProjectsDo(cb: TProjectCallback);
      procedure FilesDo(cb: TFileCallback);
      procedure FindFiles(cb: TFileCallback; query: String);

      property IsDebug : Boolean write _isDebug default False;
      property DatabasePath : String write SetDatabasePath;
  end;

implementation

constructor TDbfIndexedStorage.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  _solutionsTable := TDbf.Create(self);
  _solutionsTable.TableLevel := 7;
  _solutionsTable.TableName := 'solutions.dbf';
  _projectsTable := TDbf.Create(self);
  _projectsTable.TableLevel := 7;
  _projectsTable.TableName := 'projects.dbf';
  _filesTable := TDbf.Create(self);
  _filesTable.TableLevel := 7;
  _filesTable.TableName := 'files.dbf';
end;

procedure TDbfIndexedStorage.CreateTables;
begin
  Log('create tables');
  ForceDirectories(_databasePath);
  CreateSolutionsTable;
  CreateProjectsTable;
  CreateFilesTable;
  Log('create tables done');
end;

function TDbfIndexedStorage.AppendSolution(title: String): Integer;
begin
  Log('append solution ' + title);
  with _solutionsTable do begin
    Open;
    Append;
    FieldByName('title').AsString := title;
    FieldByName('title_index').AsString := NameToIndex(title);
    Post;
    Result := FieldByName('solution_id').AsInteger;
    Log(Format('solution #%d appended', [Result]));
    Close;
  end;
end;

function TDbfIndexedStorage.AppendProject(
  title: String;
  path: String;
  solutionId: Integer): Integer;
begin
  Log('append project ' + title);
  with _projectsTable do begin
    Open;
    Append;
    FieldByName('title').AsString := title;
    FieldByName('title_index').AsString := NameToIndex(title);
    FieldByName('path').AsString := path;
    FieldByName('solution_id').AsInteger := solutionId;
    Post;
    Result := FieldByName('project_id').AsInteger;
    Log(Format('project #%d appended', [Result]));
    Close;
  end;
end;

function TDbfIndexedStorage.AppendFile(
  title: String;
  path: String;
  solutionId: Integer;
  projectId: Integer): Integer;
begin
  Log('append file ' + title);
  with _filesTable do begin
    Open;
    Append;
    FieldByName('title').AsString := title;
    FieldByName('title_index').AsString := NameToIndex(title);
    FieldByName('path').AsString := path;
    FieldByName('solution_id').AsInteger := solutionId;
    FieldByName('project_id').AsInteger := projectId;
    Post;
    Result := FieldByName('file_id').AsInteger;
    Log(Format('file #%d appended', [Result]));
    Close;
  end;
end;


procedure TDbfIndexedStorage.SolutionsDo(cb: TSolutionCallback);
var
  solution: SolutionEntity;
begin
  Log('solutions do');
  with _solutionsTable do begin
    Open;
    try
      First;
      while not EOF do begin
        solution.Id := FieldByName('solution_id').AsInteger;
        solution.Title := FieldByName('title').AsString;
        solution.TitleIndex := FieldByName('title_index').AsString;
        solution.Path := ''; // TODO do I need it?
        cb(solution);
        Next;
      end;
    finally
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.ProjectsDo(cb: TProjectCallback);
var
  project: ProjectEntity;
begin
  Log('projects do');
  with _projectsTable do begin
    Open;
    try
      First;
      while not EOF do begin
        project.Id := FieldByName('project_id').AsInteger;
        project.Title := FieldByName('title').AsString;
        project.TitleIndex := FieldByName('title_index').AsString;
        project.Path := FieldByName('path').AsString;
        project.SolutionId := FieldByName('solution_id').AsInteger;
        cb(project);
        Next;
      end;
    finally
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.FilesDo(cb: TFileCallback);
var
  aFile: FileEntity;
begin
  Log('files do');
  with _filesTable do begin
    Open;
    try
      First;
      while not EOF do begin
        aFile.Id := FieldByName('file_id').AsInteger;
        aFile.Title := FieldByName('title').AsString;
        aFile.TitleIndex := FieldByName('title_index').AsString;
        aFile.Path := FieldByName('path').AsString;
        aFile.SolutionId := FieldByName('solution_id').AsInteger;
        aFile.ProjectId := FieldByName('project_id').AsInteger;
        cb(aFile);
        Next;
      end;
    finally
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.FindFiles(cb: TFileCallback; query: String);
var
  aFile: FileEntity;
begin
  with _filesTable do begin
    Open;
    FilterOptions := [foCaseInsensitive];
    Filter := 'title=' + QuotedStr(query);
    Filtered := True;
    try
      First;
      while not EOF do begin
        aFile.Id := FieldByName('file_id').AsInteger;
        aFile.Title := FieldByName('title').AsString;
        aFile.TitleIndex := FieldByName('title_index').AsString;
        aFile.Path := FieldByName('path').AsString;
        aFile.SolutionId := FieldByName('solution_id').AsInteger;
        aFile.ProjectId := FieldByName('project_id').AsInteger;
        cb(aFile);
        Next;
      end;
    finally
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.CreateSolutionsTable;
begin
  Log('create table solutions');
  with _solutionsTable do begin
    try
      Exclusive := True;
      Log('solutions add columns');
      with FieldDefs do begin
        Add('solution_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
        Add('title_index', ftString, 50, True);
      end;
      Log('solutions write to disk');
      CreateTable;
      Open;
      Log('solutions index');
      AddIndex('ix_s_sid', 'solution_id', [ixPrimary, ixUnique]);
      AddIndex('ix_s_title', 'title', [ixCaseInsensitive, ixUnique]);
      AddIndex('ix_s_titleix', 'title_index', [ixCaseInsensitive]);
    finally
      Log('close table solutions');
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.CreateProjectsTable;
begin
  Log('create table projects');
  with _projectsTable do begin
    try
      Exclusive := True;
      Log('projects add columns');
      with FieldDefs do begin
        Add('project_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
        Add('title_index', ftString, 50, True);
        Add('path', ftString, 255, True);
        Add('solution_id', ftInteger, 0, True);
      end;
      Log('projects write to disk');
      CreateTable;
      Open;
      Log('projects index');
      AddIndex('ix_p_pid', 'project_id', [ixPrimary, ixUnique]);
      AddIndex('ix_p_title', 'title', [ixCaseInsensitive, ixUnique]);
      AddIndex('ix_p_titleix', 'title_index', [ixCaseInsensitive]);
      AddIndex('ix_ps_sid', 'solution_id', []);
    finally
      Log('close table projects');
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.CreateFilesTable;
begin
  Log('create table files');
  with _filesTable do begin
    try
      Exclusive := True;
      Log('files add columns');
      with FieldDefs do begin
        Add('file_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
        Add('title_index', ftString, 50, True);
        Add('path', ftString, 255, True);
        Add('solution_id', ftInteger, 0, True);
        Add('project_id', ftInteger, 0, True);
      end;
      Log('files write to disk');
      CreateTable;
      Open;
      Log('files index');
      AddIndex('ix_f_f_id', 'file_id', [ixPrimary, ixUnique]);
      AddIndex('ix_f_title', 'title', [ixCaseInsensitive]);
      AddIndex('ix_f_titleix', 'title_index', [ixCaseInsensitive]);
      AddIndex('ix_fs_sid', 'solution_id', []);
      AddIndex('ix_fp_pid', 'project_id', []);
    finally
      Log('close table files');
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.SetDatabasePath(path: String);
begin
  _databasePath := AppendPathDelim(path);
  _solutionsTable.FilePath := _databasePath;
  _projectsTable.FilePath := _databasePath;
  _filesTable.FilePath := _databasePath;
end;

procedure TDbfIndexedStorage.Log(message: String);
begin
  if _isDebug then DebugLn('%s %s', [ClassName, message]);
end;

end.

