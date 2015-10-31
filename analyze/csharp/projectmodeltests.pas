unit ProjectModelTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  ProjectModel;

type

  TProjectModelTests= class(TTestCase)
  private
    _project: TProjectModel;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure Load;
  end;

implementation

procedure TProjectModelTests.Load;
begin
  _project.Load('..\testdata\analyze\csharp\SimpleWallet.Core\SimpleWallet.Core.csproj');
  AssertEquals('Title', 'SimpleWallet.Core', _project.Title);
end;

procedure TProjectModelTests.SetUp;
begin
  _project := TProjectModel.Create(nil);
end;

procedure TProjectModelTests.TearDown;
begin
  FreeAndNil(_project);
end;

initialization

  RegisterTest(TProjectModelTests);
end.

