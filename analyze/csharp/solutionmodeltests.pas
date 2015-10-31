unit SolutionModelTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  SolutionModel;

type

  TSolutionModelTests= class(TTestCase)
  private
    _solution: TSolutionModel;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLoad;
  end;

implementation

procedure TSolutionModelTests.TestLoad;
begin
  _solution.Load('..\testdata\analyze\csharp\SimpleWallet.sln');
  AssertEquals('Name', 'SimpleWallet', _solution.Name);
end;

procedure TSolutionModelTests.SetUp;
begin
  _solution := TSolutionModel.Create(nil);
end;

procedure TSolutionModelTests.TearDown;
begin
  FreeAndNil(_solution);
end;

initialization

  RegisterTest(TSolutionModelTests);
end.

