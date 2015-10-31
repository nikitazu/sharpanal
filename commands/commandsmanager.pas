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
  _commands.Add(UpperCase(TConfigCommand.ClassName), TConfigCommand.ClassType);
  _commands.Add(UpperCase(TInitCommand.ClassName), TInitCommand.ClassType);
  _commands.Add(UpperCase(TLinkCommand.ClassName), TLinkCommand.ClassType);
  _commands.Add(UpperCase(TUpdateCommand.ClassName), TUpdateCommand.ClassType);
  _commands.Add(UpperCase(THelpCommand.ClassName), THelpCommand.ClassType);
  WriteLn('create commands done');
end;

class destructor TCommandsManager.Destroy;
begin
  WriteLn('destroy commands');
  FreeAndNil(_commands);
end;

class function TCommandsManager.Find(name: ShortString): TAbstractCommandClass;
var
  commandName: ShortString;
begin
  commandName := 'T' + UpperCase(name) + 'COMMAND';
  Result := TAbstractCommandClass(_commands.Find(commandName));
  if Result = nil then Result := TAbstractCommandClass(THelpCommand.ClassType);
end;

end.

