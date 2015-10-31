unit LinkCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  AbstractCommand;

type
  TLinkCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
    protected
      procedure OnRun; override;
      function ShortOptions: String; override;
      function LongOptions: String; override;
  end;

implementation

class function TLinkCommand.CommandName: ShortString;
begin
  Result := 'link';
end;

procedure TLinkCommand.OnRun;
var
  projectName: String;
  pathToSolution: String;
begin
  projectName := _app.GetOptionValue('n','name');
  pathToSolution := _app.GetOptionValue('p','path');

  Log('start: ' + pathToSolution);

  if AssertNotEmpty(projectName, 'name', 'name should be the same as in init command')
  and AssertNotEmpty(pathToSolution, 'path', 'path should lead to Visual Studio solution file')
  and AssertDirExists(_config.GetDatabasePath(projectName), 'name should be the same as in init command')
  and AssertFileExists(pathToSolution, 'path should lead to Visual Studio solution file')
  then begin
    _config.SetSolutionPath(projectName, pathToSolution);
  end;
end;

function TLinkCommand.ShortOptions: String;
begin
  Result := 'n:p:';
end;

function TLinkCommand.LongOptions: String;
begin
  Result := 'name: path:';
end;

end.

