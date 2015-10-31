unit SolutionModelTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TSolutionModelTests= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TSolutionModelTests.TestHookUp;
begin
  Fail('Utter failure');
end;

procedure TSolutionModelTests.SetUp;
begin

end;

procedure TSolutionModelTests.TearDown;
begin

end;

initialization

  RegisterTest(TSolutionModelTests);
end.

