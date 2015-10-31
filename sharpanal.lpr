program sharpanal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, strutils, CustApp, FileUtil,
  { you can add units after this }
  Configuration,
  InitCommand, LinkCommand, ConfigCommand, UpdateIndexCommand;

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
  ErrorMsg: String;
  commandName: String;
begin
  if ParamCount < 1 then begin
    WriteLn('no command specified, abort');
    Terminate;
    Exit;
  end;

  commandName := ParamStr(1);
  if AnsiStartsStr('-', commandName) then begin
    WriteLn('wrong command name ',
      commandName,
      ', options should follow after command name');
    Terminate;
    Exit;
  end;

  case commandName of
  'config':
    begin
      ErrorMsg:=CheckOptions('hk:v:','help key: value:');
      if ErrorMsg<>'' then ShowException(Exception.Create(ErrorMsg))
      else ConfigCommand.Run(GetOptionValue('k','key'));
      Terminate;
      Exit;
    end;
  'init':
    begin
      ErrorMsg:=CheckOptions('hn:','help name:');
      if ErrorMsg<>'' then ShowException(Exception.Create(ErrorMsg))
      else InitCommand.Run(GetOptionValue('n','name'));
      Terminate;
      Exit;
    end;
  'link':
    begin
      ErrorMsg:=CheckOptions('hn:p:','help name: path:');
      if ErrorMsg<>'' then ShowException(Exception.Create(ErrorMsg))
      else LinkCommand.Run(GetOptionValue('n','name'), GetOptionValue('p','path'));
      Terminate;
      Exit;
    end;
  'update':
    begin
      ErrorMsg:=CheckOptions('hn:','help name:');
      if ErrorMsg<>'' then ShowException(Exception.Create(ErrorMsg))
      else UpdateIndexCommand.Run(GetOptionValue('n','name'));
      Terminate;
      Exit;
    end;
  end;

  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  // no commands found, print usage
  // TODO: replace it with interactive mode
  WriteHelp;
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

