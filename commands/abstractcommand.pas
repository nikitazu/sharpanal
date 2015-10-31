unit AbstractCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp;

type
  TAbstractCommand = class(TObject)
    public
      constructor Create(name: String; app: TCustomApplication);
      destructor Destroy; override;
      procedure Run; virtual;
    protected
      _name: String;
      _app: TCustomApplication;
  end;

implementation

constructor TAbstractCommand.Create(name: String; app: TCustomApplication);
begin
  _name := name;
  _app := app;
  WriteLn('command [', _name, '] create');
  WriteLn('command [', _name, '] create done');
end;

destructor TAbstractCommand.Destroy;
begin
  WriteLn('command [', _name, '] destroyed');
end;

procedure TAbstractCommand.Run;
begin
  WriteLn('run');
end;

end.

