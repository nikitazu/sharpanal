unit FileModelTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  FileUtil,
  FileModel;

type

  TFileModelTests= class(TTestCase)
  private
    _file: TFileModel;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Load;
  end;

implementation

procedure TFileModelTests.Load;
begin
  _file.Load(
    AppendPathDelim('..') +
    AppendPathDelim('testdata') +
    AppendPathDelim('analyze') +
    AppendPathDelim('csharp') +
    AppendPathDelim('SimpleWallet.Core') +
    AppendPathDelim('Exporters') +
    'QifExporter.cs');
  AssertEquals('Title', 'QifExporter', _file.Title);
end;

procedure TFileModelTests.SetUp;
begin
  _file := TFileModel.Create(nil);
end;

procedure TFileModelTests.TearDown;
begin
  FreeAndNil(_file);
end;

initialization

  RegisterTest(TFileModelTests);
end.

