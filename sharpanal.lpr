program sharpanal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  strutils,
  AbstractCommand,
  CommandsManager;

type

  { TSharpAnal }

  TSharpAnal = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
    WriteLn(StdErr,
      Format('ERROR: wrong command name %s options should follow after command name',
      [commandName]));
    Terminate;
    Exit;
  end;

  command := TCommandsManager.Find(commandName).Create(self);
  command.Run;

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

var
  Application: TSharpAnal;
begin
  Application:=TSharpAnal.Create(nil);
  Application.Title:='Sharp anal';
  Application.Run;
  Application.Free;
end.

