unit InitCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  Configuration;

procedure Run(projectName: String);

implementation

procedure Run(projectName: String);
var
  config: TConfig;
  databasePath: String;
begin
  writeln('init start: ' + projectName);
  config := TConfig.Create;
  databasePath := config.GetDatabasePath(projectName);
  if DirectoryExistsUTF8(databasePath) or FileExistsUTF8(databasePath) then
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


