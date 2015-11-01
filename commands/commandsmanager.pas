unit CommandsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  AbstractCommand,
  ConfigCommand,
  InitCommand,
  LinkCommand,
  UpdateCommand,
  FindCommand,
  HelpCommand;

type
  TCommandsManager = Class(TObject)
    public
      class constructor Create;
      class destructor Destroy;
      class function Find(name: ShortString): TAbstractCommandClass;
    private
    class var
      _commands: TFPHashList;
  end;


implementation

class constructor TCommandsManager.Create;
begin
  WriteLn('create commands');
  _commands := TFPHashList.Create;
  _commands.Add(TConfigCommand.CommandName, TConfigCommand.ClassType);
  _commands.Add(TInitCommand.CommandName, TInitCommand.ClassType);
  _commands.Add(TLinkCommand.CommandName, TLinkCommand.ClassType);
  _commands.Add(TUpdateCommand.CommandName, TUpdateCommand.ClassType);
  _commands.Add(TFindCommand.CommandName, TFindCommand.ClassType);
  _commands.Add(THelpCommand.CommandName, THelpCommand.ClassType);
end;

class destructor TCommandsManager.Destroy;
begin
  WriteLn('destroy commands');
  FreeAndNil(_commands);
end;

class function TCommandsManager.Find(name: ShortString): TAbstractCommandClass;
begin
  Result := TAbstractCommandClass(_commands.Find(name));
  if Result = nil then Result := TAbstractCommandClass(THelpCommand.ClassType);
end;

end.

