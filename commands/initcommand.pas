unit InitCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, strutils,
  Configuration, AbstractCommand;

type
  TInitCommand = class(TAbstractCommand)
    public
      procedure Run; override;
  end;

implementation

procedure TInitCommand.Run;
var
  projectName: String;
  error: String;
  config: TConfig;
  databasePath: String;
begin

  error := _app.CheckOptions('hn:','help name:');
  if error <> '' then begin
     _app.ShowException(Exception.Create(error));
     _app.Terminate;
     Exit;
  end;

  projectName := _app.GetOptionValue('n','name');

  writeln('init start: ' + projectName);
  config := TConfig.Create;
  databasePath := config.GetDatabasePath(projectName);
  if IsEmptyStr(Trim(projectName), [#9]) then
  begin
    writeln('init error: missing argument - name');
  end
  else if DirectoryExistsUTF8(databasePath) or FileExistsUTF8(databasePath) then
  begin
    writeln('init error: path already exists - ' + databasePath);
    writeln('init hint: initialize to not yet created directory');
  end
  else
  begin
    writeln('create dir: ' + databasePath);
    if CreateDirUTF8(databasePath) then writeln('init done')
    else WriteLn('init error: unable to create ' + databasePath);
  end;
end;

end.


