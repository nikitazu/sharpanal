unit AbstractCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp,
  strutils, FileUtil,
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

      function AssertNotEmpty(
        optionValue: String;
        optionName: String;
        hint: String): Boolean;

      function AssertPathNotExists(
        path: String;
        hint: String): Boolean;

      function AssertDirExists(
        path: String;
        hint: String): Boolean;

      function AssertFileExists(
        path: String;
        hint: String): Boolean;
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

function TAbstractCommand.AssertNotEmpty(
  optionValue: String;
  optionName: String;
  hint: String): Boolean;
begin
  Result := not IsEmptyStr(Trim(optionValue), [#9]);
  if not Result then
  Error(Format(
    'option %s expects not empty value%shint: %s',
    [optionName, LineEnding, hint]));
end;

function TAbstractCommand.AssertPathNotExists(
  path: String;
  hint: String): Boolean;
begin
  Result := not DirectoryExistsUTF8(path) and not FileExistsUTF8(path);
  if not Result then
  Error(Format(
    'path %s already exists%shint: %s',
    [path, LineEnding, hint]));
end;

function TAbstractCommand.AssertDirExists(
  path: String;
  hint: String): Boolean;
begin
  Result := DirectoryExistsUTF8(path);
  if not Result then
  Error(Format('directory not found %s%shint: %s', [path, LineEnding, hint]));
end;

function TAbstractCommand.AssertFileExists(
  path: String;
  hint: String): Boolean;
begin
  Result := FileExistsUTF8(path);
  if not Result then
  Error(Format('file not found %s%shint: %s', [path, LineEnding, hint]));
end;

end.

