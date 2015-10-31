unit InitCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, strutils,
  Configuration, AbstractCommand;

type
  TInitCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
    protected
      procedure OnRun; override;
  end;

implementation

class function TInitCommand.CommandName: ShortString;
begin
  Result := 'init';
end;

procedure TInitCommand.OnRun;
var
  projectName: String;
  argsError: String;
  config: TConfig;
  databasePath: String;
begin
  argsError := _app.CheckOptions('hvn:','help verbose name:');
  if argsError <> '' then begin
     _app.ShowException(Exception.Create(argsError));
     _app.Terminate;
     Exit;
  end;

  projectName := _app.GetOptionValue('n','name');

  Log('start: ' + projectName);
  config := TConfig.Create;
  databasePath := config.GetDatabasePath(projectName);
  if IsEmptyStr(Trim(projectName), [#9]) then
  begin
    Error('missing argument - name');
  end
  else if DirectoryExistsUTF8(databasePath) or FileExistsUTF8(databasePath) then
  begin
    Error('path already exists - ' + databasePath);
    Log('hint: initialize to not yet created directory');
  end
  else
  begin
    Log('create dir: ' + databasePath);
    if not CreateDirUTF8(databasePath)
    then Error('unable to create ' + databasePath);
  end;
end;

end.


