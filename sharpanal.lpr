program sharpanal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, strutils, CustApp, FileUtil,
  { you can add units after this }
  Configuration,
  AbstractCommand,
  InitCommand, LinkCommand, ConfigCommand, UpdateIndexCommand, HelpCommand;

type

  { TSharpAnal }

  TSharpAnal = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TSharpAnal }

procedure TSharpAnal.DoRun;
var
  commandName: String;
  command: TAbstractCommand;
begin
  if ParamCount < 1 then
    commandName := ''
  else
    commandName := ParamStr(1);

  if AnsiStartsStr('-', commandName) then begin
    WriteLn('wrong command name ',
      commandName,
      ', options should follow after command name');
    Terminate;
    Exit;
  end;

  command := nil;
  case commandName of
  'config': command := TConfigCommand.Create(commandName, self);
  'init':   command := TInitCommand.Create(commandName, self);
  'link':   command := TLinkCommand.Create(commandName, self);
  'update': command := TUpdateCommand.Create(commandName, self);
  otherwise command := THelpCommand.Create('help', self);
  end;

  if command <> nil then command.Run;

  // stop program loop
  Terminate;
end;

constructor TSharpAnal.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TSharpAnal.Destroy;
begin
  inherited Destroy;
end;

procedure TSharpAnal.WriteHelp;
var
  filename: String;
begin
  filename := ExtractFileNameOnly(ExeName);
  writeln('Usage: ',filename,' -h');
  writeln(filename,' config --key foo');
  writeln(filename,' init --name db');
  writeln(filename,' link --name db --path vs/projects/solution.sln');
  writeln(filename,' update --name db');
end;

var
  Application: TSharpAnal;
begin
  Application:=TSharpAnal.Create(nil);
  Application.Title:='Sharp anal';
  Application.Run;
  Application.Free;
end.

