unit UpdateCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils,
  Configuration, AbstractCommand;

type
  TUpdateCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
    protected
      procedure OnRun; override;
      function ShortOptions: String; override;
      function LongOptions: String; override;
  end;

implementation

class function TUpdateCommand.CommandName: ShortString;
begin
  Result := 'update';
end;

procedure TUpdateCommand.OnRun;
var
  projectName: String;
  config: TConfig;
  databasePath: String;
begin
  projectName := _app.GetOptionValue('n','name');
  Log('start: ' + projectName);
  config := TConfig.Create;
  databasePath := config.GetDatabasePath(projectName);
  if IsEmptyStr(Trim(projectName), [#9]) then
  begin
    Error('missing argument - name');
    Log('hint: name should be the same as in init command');
  end;
end;

function TUpdateCommand.ShortOptions: String;
begin
  Result := 'n:';
end;

function TUpdateCommand.LongOptions: String;
begin
  Result := 'name:';
end;

end.

