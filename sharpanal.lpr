program sharpanal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, FileUtil,
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
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hcilun','help config init link update name');
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

  if HasOption('c','config') then begin
    ConfigCommand.Run(GetOptionValue('c','config'));
    Terminate;
    Exit;
  end;

  if HasOption('i','init') then begin
    InitCommand.Run(GetOptionValue('i','init'));
    Terminate;
    Exit;
  end;

  if HasOption('l','link') then begin
    LinkCommand.Run(GetOptionValue('n','name'), GetOptionValue('l','link'));
    Terminate;
    Exit;
  end;

  if HasOption('u','update') then begin
    UpdateIndexCommand.Run(GetOptionValue('u','update'));
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
  writeln(filename,' --config key');
  writeln(filename,' --init dbname');
  writeln(filename,' --link path/to/solution.sln --name dbname');
  writeln(filename,' --update dbname');
end;

var
  Application: TSharpAnal;
begin
  Application:=TSharpAnal.Create(nil);
  Application.Title:='Sharp anal';
  Application.Run;
  Application.Free;
end.

