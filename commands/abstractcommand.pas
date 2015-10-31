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
      procedure Run; virtual;
    protected
      procedure Log(message: String);
      procedure Error(message: String);
    protected
      _app: TCustomApplication;
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
end;

procedure TAbstractCommand.Log(message: String);
begin
  if _isDebug then DebugLn('%s %s', [ClassName, message]);
end;

procedure TAbstractCommand.Error(message: String);
begin
  WriteLn(StdErr, Format('%s ERROR: %s', [ClassName, message]));
end;

end.

