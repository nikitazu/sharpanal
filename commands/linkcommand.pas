unit LinkCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Run(pathToSolution: String);

implementation

procedure Run(pathToSolution: String);
begin
  writeln('link start');
  writeln('link path: ' + pathToSolution);
  if not FileExists(pathToSolution) then
  begin
    writeln('link error: file not found - ' + pathToSolution);
    writeln('link hint: path should lead to Visual Studio solution file');
  end
  else
  begin
    writeln('link done');
  end;
end;

end.

