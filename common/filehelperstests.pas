unit FileHelpersTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  FileUtil,
  FileHelpers;

type

  TFileHelpersTests= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestPathCombine;
    procedure TestPathCombineEmpty;
  end;

implementation

procedure TFileHelpersTests.TestPathCombine;
begin
  AssertEquals('Path combine foo+bar',
    AppendPathDelim('foo') + 'bar',
    PathCombine(['foo', 'bar']));

  AssertEquals('Path combine foo+bar+buz',
    AppendPathDelim('foo') + AppendPathDelim('bar') + 'buz',
    PathCombine(['foo', 'bar', 'buz']));
end;

procedure TFileHelpersTests.TestPathCombineEmpty;
begin
  AssertEquals('Path combine empty', '', PathCombine([]));
end;

procedure TFileHelpersTests.SetUp;
begin

end;

procedure TFileHelpersTests.TearDown;
begin

end;

initialization

  RegisterTest(TFileHelpersTests);
end.

