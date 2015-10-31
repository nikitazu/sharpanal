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
      procedure Run; override;
  end;

implementation

class function TUpdateCommand.CommandName: ShortString;
begin
  Result := 'update';
end;

procedure TUpdateCommand.Run;
var
  argsError: String;
  projectName: String;
  config: TConfig;
  databasePath: String;
begin
  inherited;
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
    Log('hint: name should be the same as in init command');
  end;
  Log('done');
end;

end.

