unit InitCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Run(path: String);

implementation

procedure Run(path: String);
begin
  writeln('init start');
  writeln('init path: ' + path);
  if DirectoryExists(path) or FileExists(path) then
  begin
    writeln('init error: path already exists - ' + path);
    writeln('init hint: initialize to not yet created directory');
  end
  else
  begin
    writeln('create dir: ' + path);
    CreateDir(path);
    writeln('init done');
  end;
end;

end.


