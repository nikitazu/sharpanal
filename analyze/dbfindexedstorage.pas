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
  Log('TDbfIndexedStorage create tables');
  ForceDirectories(path);
  Log('TDbfIndexedStorage create table solutions');
  CreateSolutionsTable(path);
  Log('TDbfIndexedStorage create table projects');
  CreateProjectsTable(path);
  Log('TDbfIndexedStorage create table files');
  CreateFilesTable(path);
  Log('TDbfIndexedStorage create tables done');
end;

procedure TDbfIndexedStorage.CreateSolutionsTable(path: String);
begin
  with _solutionsTable do begin
    try
      FilePath := AppendPathDelim(path);
      TableLevel := 7;
      Exclusive := True;
      TableName := 'solutions.dbf';
      Log('TDbfIndexedStorage solutions add columns');
      with FieldDefs do begin
        Add('solution_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
      end;
      Log('TDbfIndexedStorage solutions write to disk');
      CreateTable;
      Open;
      Log('TDbfIndexedStorage solutions index');
      AddIndex('ix_s_sid', 'solution_id', [ixPrimary, ixUnique]);
      AddIndex('ix_s_title', 'title', [ixCaseInsensitive, ixUnique]);
    finally
      Log('TDbfIndexedStorage close table solutions');
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
      Log('TDbfIndexedStorage projects add columns');
      with FieldDefs do begin
        Add('project_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
        Add('solution_id', ftInteger, 0, True);
      end;
      Log('TDbfIndexedStorage projects write to disk');
      CreateTable;
      Open;
      Log('TDbfIndexedStorage projects index');
      AddIndex('ix_p_pid', 'project_id', [ixPrimary, ixUnique]);
      AddIndex('ix_p_title', 'title', [ixCaseInsensitive, ixUnique]);
      AddIndex('ix_ps_sid', 'solution_id', []);
    finally
      Log('TDbfIndexedStorage close table projects');
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
      Log('TDbfIndexedStorage files add columns');
      with FieldDefs do begin
        Add('file_id', ftAutoInc, 0, True);
        Add('title', ftString, 100, True);
        Add('solution_id', ftInteger, 0, True);
        Add('project_id', ftInteger, 0, True);
      end;
      Log('TDbfIndexedStorage files write to disk');
      CreateTable;
      Open;
      Log('TDbfIndexedStorage files index');
      AddIndex('ix_f_f_id', 'file_id', [ixPrimary, ixUnique]);
      AddIndex('ix_f_title', 'title', [ixCaseInsensitive, ixUnique]);
      AddIndex('ix_fs_sid', 'solution_id', []);
      AddIndex('ix_fp_pid', 'project_id', []);
    finally
      Log('TDbfIndexedStorage close table files');
      Close;
    end;
  end;
end;

procedure TDbfIndexedStorage.Log(message: String);
begin
  if _isDebug then DebugLn('%s %s', [ClassName, message]);
end;

end.

