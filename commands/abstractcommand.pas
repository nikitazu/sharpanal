unit AbstractCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp;

type
  TAbstractCommand = class(TComponent)
    public
      constructor Create(aOwner: TComponent); override;
      constructor Create(app: TCustomApplication);
      destructor Destroy; override;
      procedure Run; virtual;
    protected
      _app: TCustomApplication;
  end;

  TAbstractCommandClass = Class of TAbstractCommand;

implementation

constructor TAbstractCommand.Create(aOwner: TComponent);
begin
  inherited;
end;

constructor TAbstractCommand.Create(app: TCustomApplication);
begin
  WriteLn('create command [', ClassName, ']');
  inherited Create(app);
  _app := app;
end;

destructor TAbstractCommand.Destroy;
begin
  WriteLn('destroy command [', ClassName, ']');
  _app := nil; // app is the owner do not free it
  inherited;
end;

procedure TAbstractCommand.Run;
begin
  WriteLn('run');
end;

end.

