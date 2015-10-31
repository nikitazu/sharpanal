unit AbstractCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp;

type
  TAbstractCommand = class(TComponent)
    public
      constructor Create(aOwner: TComponent); override;
      constructor Create(app: TCustomApplication; aName: String);
      destructor Destroy; override;
      procedure Run; virtual;
    protected
      _name: String;
      _app: TCustomApplication;
  end;

implementation

constructor TAbstractCommand.Create(aOwner: TComponent);
begin
  inherited;
end;

constructor TAbstractCommand.Create(app: TCustomApplication; aName: String);
begin
  inherited Create(app);
  _name := aName;
  _app := app;
  WriteLn('command [', _name, '] create');
  WriteLn('command [', _name, '] create done');
end;

destructor TAbstractCommand.Destroy;
begin
  WriteLn('command [', _name, '] destroyed');
  inherited;
end;

procedure TAbstractCommand.Run;
begin
  WriteLn('run');
end;

end.

