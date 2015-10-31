unit InitCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Configuration, AbstractCommand;

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
  config: TConfig;
  databasePath: String;
begin
  projectName := _app.GetOptionValue('n','name');

  Log('start: ' + projectName);
  config := TConfig.Create;
  databasePath := config.GetDatabasePath(projectName);

  if AssertNotEmpty(projectName, 'name', 'name should be a string')
  and AssertPathNotExists(databasePath, 'initialize to not yet created directory')
  then begin
    Log('create dir: ' + databasePath);
    if not CreateDirUTF8(databasePath)
    then Error('unable to create ' + databasePath);
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


