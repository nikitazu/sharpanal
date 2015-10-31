unit UpdateIndexCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Configuration;

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
  writeln('update done');
end;

end.

