unit SolutionModelTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
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
  AssertEquals('Title', 'SimpleWallet', _solution.Title);
  AssertEquals('Projects count', 3, _solution.Projects.Count);

  AssertEquals('Project 1',
    'SimpleWallet\SimpleWallet.csproj',
    _solution.Projects[0]);

  AssertEquals('Project 2',
    'SimpleWallet.Core\SimpleWallet.Core.csproj',
    _solution.Projects[1]);

  AssertEquals('Project 3',
    'SimpleWallet.Core.Tests\SimpleWallet.Core.Tests.csproj',
    _solution.Projects[2]);
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

