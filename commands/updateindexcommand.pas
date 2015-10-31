unit UpdateIndexCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  Configuration;

procedure Run(projectName: String);

implementation

procedure Run(projectName: String);
var
  config: TConfig;
  databasePath: String;
begin
  writeln('update start: ' + projectName);
  config := TConfig.Create;
  databasePath := config.GetDatabasePath(projectName);
  if IsEmptyStr(Trim(projectName), [#9]) then
  begin
    writeln('update error: missing argument - name');
    writeln('update hint: name should be the same as in init command');
  end;
  writeln('update done');
end;

end.

