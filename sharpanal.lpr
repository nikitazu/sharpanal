program sharpanal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, InitCommand, LinkCommand, ConfigCommand
  { you can add units after this };

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
  ErrorMsg:=CheckOptions('hcil','help config init link');
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
    LinkCommand.Run(GetOptionValue('l','link'));
    Terminate;
    Exit;
  end;

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
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TSharpAnal;
begin
  Application:=TSharpAnal.Create(nil);
  Application.Title:='Sharp anal';
  Application.Run;
  Application.Free;
end.

