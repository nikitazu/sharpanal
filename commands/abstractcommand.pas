unit AbstractCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp,
  LazLogger;

type
  TAbstractCommand = class(TComponent)
    public
      constructor Create(aOwner: TComponent); override;
      constructor Create(app: TCustomApplication);
      destructor Destroy; override;
      procedure Run;
    protected
      procedure OnRun; virtual; abstract;
      procedure Log(message: String);
      procedure Error(message: String);
      function ShortOptions: String; virtual;
      function LongOptions: String; virtual;
    protected
      _app: TCustomApplication;
      _optionsError: String;
    private
      _isDebug: Boolean;
  end;

  TAbstractCommandClass = Class of TAbstractCommand;

implementation

constructor TAbstractCommand.Create(aOwner: TComponent);
begin
  inherited;
end;

constructor TAbstractCommand.Create(app: TCustomApplication);
begin
  inherited Create(app);
  _app := app;
  _optionsError := '';
  _isDebug := _app.HasOption('v','verbose');
  Log('create');
end;

destructor TAbstractCommand.Destroy;
begin
  Log('destroy');
  _app := nil; // app is the owner do not free it
  inherited;
end;

procedure TAbstractCommand.Run;
begin
  Log('run');

  _optionsError := _app.CheckOptions(
    'hv' + ShortOptions,
    'help verbose ' + LongOptions);

  if _optionsError <> '' then begin
     _app.ShowException(Exception.Create(_optionsError));
     _app.Terminate;
     Exit;
  end;

  OnRun;
  Log('done');
end;

procedure TAbstractCommand.Log(message: String);
begin
  if _isDebug then DebugLn('%s %s', [ClassName, message]);
end;

procedure TAbstractCommand.Error(message: String);
begin
  WriteLn(StdErr, Format('%s ERROR: %s', [ClassName, message]));
end;

function TAbstractCommand.ShortOptions: String;
begin
  Result := '';
end;

function TAbstractCommand.LongOptions: String;
begin
  Result := '';
end;

end.

