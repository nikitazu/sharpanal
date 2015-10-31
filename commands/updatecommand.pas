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
  error: String;
  projectName: String;
  config: TConfig;
  databasePath: String;
begin
  inherited;
  error := _app.CheckOptions('hn:','help name:');
  if error <> '' then begin
     _app.ShowException(Exception.Create(error));
     _app.Terminate;
     Exit;
  end;

  projectName := _app.GetOptionValue('n','name');
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

