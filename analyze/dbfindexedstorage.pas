unit DbfIndexedStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil, LazLogger,
  dbf, db;

type
  TDbfIndexedStorage = Class(TComponent)
    private
      _isDebug: Boolean;
      _solutionsTable: TDbf;
      _projectsTable: TDbf;
      _filesTable: TDbf;
    private
      procedure CreateSolutionsTable(path: String);
      procedure CreateProjectsTable(path: String);
      procedure CreateFilesTable(path: String);
      procedure Log(message: String);
    public
      constructor Create(aOwner: TComponent); override;
      procedure CreateTables(path: String);

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

      property IsDebug : Boolean write _isDebug;
  end;

implementation

constructor TDbfIndexedStorage.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  _isDebug := False;
  _solutionsTable := TDbf.Create(self);
  _projectsTable := TDbf.Create(self);
  _filesTable := TDbf.Create(self);
end;

procedure TDbfIndexedStorage.CreateTables(path: String);
begin
  Log('create tables');
  ForceDirectories(path);
  Log('create table solutions');
  CreateSolutionsTable(path);
  Log('create table projects');
  CreateProjectsTable(path);
  Log('create table files');
  CreateFilesTable(path);
  Log('create tables done');
end;

function TDbfIndexedStorage.AppendSolution(title: String): Integer;
begin
  Log('append solution ' + title);
  with _solutionsTable do begin
    AppendRecord([nil, title]);
    Result := FieldValues['solution_id'];
  end;
end;

function TDbfIndexedStorage.AppendProject(
  title: String;
  path: String;
  solutionId: Integer): Integer;
begin
  Log('append project ' + title);
  with _projectsTable do begin
    AppendRecord([nil, title, path, solutionId]);
    Result := FieldValues['project_id'];
  end;
end;

function TDbfIndexedStorage.AppendFile(
  title: String;
  path: String;
  solutionId: Integer;
  projectId: Integer): Integer;
begin
  Log('append file ' + title);
  with _projectsTable do begin
    AppendRecord([nil, title, path, solutionId, projectId]);
    Result := FieldValues['file_id'];
  end;
end;

procedure TDbfIndexedStorage.CreateSolutionsTable(path: String);
begin
  with _solutionsTable do begin
    try
      FilePath := AppendPathDelim(path);
      TableLevel := 7;
      Exclusive := True;
      TableName := 'solutions.dbf';
      Log('solutions add columns');
      with FieldDefs do begin
        Add('solution_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
      end;
      Log('solutions write to disk');
      CreateTable;
      Open;
      Log('solutions index');
      AddIndex('ix_s_sid', 'solution_id', [ixPrimary, ixUnique]);
      AddIndex('ix_s_title', 'title', [ixCaseInsensitive, ixUnique]);
    finally
      Log('close table solutions');
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.CreateProjectsTable(path: String);
begin
  with _projectsTable do begin
    try
      FilePath := AppendPathDelim(path);
      TableLevel := 7;
      Exclusive := True;
      TableName := 'projects.dbf';
      Log('projects add columns');
      with FieldDefs do begin
        Add('project_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
        Add('path', ftString, 255, True);
        Add('solution_id', ftInteger, 0, True);
      end;
      Log('projects write to disk');
      CreateTable;
      Open;
      Log('projects index');
      AddIndex('ix_p_pid', 'project_id', [ixPrimary, ixUnique]);
      AddIndex('ix_p_title', 'title', [ixCaseInsensitive, ixUnique]);
      AddIndex('ix_p_path', 'path', [ixCaseInsensitive, ixUnique]);
      AddIndex('ix_ps_sid', 'solution_id', []);
    finally
      Log('close table projects');
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.CreateFilesTable(path: String);
begin
  with _filesTable do begin
    try
      FilePath := AppendPathDelim(path);
      TableLevel := 7;
      Exclusive := True;
      TableName := 'files.dbf';
      Log('files add columns');
      with FieldDefs do begin
        Add('file_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
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
      AddIndex('ix_f_path', 'path', [ixCaseInsensitive]);
      AddIndex('ix_fs_sid', 'solution_id', []);
      AddIndex('ix_fp_pid', 'project_id', []);
    finally
      Log('close table files');
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.Log(message: String);
begin
  if _isDebug then DebugLn('%s %s', [ClassName, message]);
end;

end.

