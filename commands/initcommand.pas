unit InitCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  AbstractCommand,
  DbfIndexedStorage;

type
  TInitCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
    protected
      procedure OnRun; override;
      function ShortOptions: String; override;
      function LongOptions: String; override;
  end;

implementation

class function TInitCommand.CommandName: ShortString;
begin
  Result := 'init';
end;

procedure TInitCommand.OnRun;
var
  projectName: String;
  databasePath: String;
  storage: TDbfIndexedStorage;
begin
  projectName := _app.GetOptionValue('n','name');

  Log('start: ' + projectName);
  databasePath := _config.GetDatabasePath(projectName);

  if AssertNotEmpty(projectName, 'name', 'name should be a string')
  and AssertPathNotExists(databasePath, 'initialize to not yet created directory')
  then begin
    Log('create dir: ' + databasePath);
    if not ForceDirectoriesUTF8(databasePath)
    then Error('unable to create ' + databasePath)
    else begin
      try
        storage := TDbfIndexedStorage.Create(self);
        storage.IsDebug := _app.HasOption('v','verbose');
        storage.CreateTables(databasePath);
      except
        on e : Exception do begin
          Error(Format('%s - %s', [e.ClassName, e.Message]));
          DeleteDirectory(databasePath, False);
        end;
      end;
    end;
  end;
end;

function TInitCommand.ShortOptions: String;
begin
  Result := 'n:';
end;

function TInitCommand.LongOptions: String;
begin
  Result := 'name:';
end;

end.


