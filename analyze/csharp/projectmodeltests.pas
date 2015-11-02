unit ProjectModelTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FileHelpers,
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
  _project.Load(PathCombine(
    ['..','testdata','analyze','csharp','SimpleWallet.Core','SimpleWallet.Core.csproj']));

  AssertEquals('Title', 'SimpleWallet.Core', _project.Title);
  AssertEquals('Files count', 19, _project.Files.Count);

  AssertEquals('File 1',
    'ComponentModel\PropertyChangedNotifyer.cs',
    _project.Files[0]);

  AssertEquals('File 2',
    'DataProcessing\DateGroup.cs',
    _project.Files[1]);

  AssertEquals('File 3',
    'DataProcessing\DateGroupType.cs',
    _project.Files[2]);

  AssertEquals('File 4',
    'DataProcessing\GroupByTime.cs',
    _project.Files[3]);

  AssertEquals('File 5',
    'DataProcessing\GroupByMonth.cs',
    _project.Files[4]);

  AssertEquals('File 6',
    'Exporters\ExportableTransaction.cs',
    _project.Files[5]);

  AssertEquals('File 7',
    'Exporters\QifExporter.cs',
    _project.Files[6]);

  AssertEquals('File 8',
    'Exporters\QifType.cs',
    _project.Files[7]);

  AssertEquals('File 9',
    'Extensions\DateTimeOffsetExt.cs',
    _project.Files[8]);

  AssertEquals('File 10',
    'Extensions\JsonObjectExt.cs',
    _project.Files[9]);

  AssertEquals('File 11',
    'Extensions\TimeSpanExt.cs',
    _project.Files[10]);

  AssertEquals('File 12',
    'Extensions\StringParseExt.cs',
    _project.Files[11]);

  AssertEquals('File 13',
    'Properties\AssemblyInfo.cs',
    _project.Files[12]);

  AssertEquals('File 14',
    'UI\Converters\BoolToVisibility.cs',
    _project.Files[13]);

  AssertEquals('File 15',
    'UI\Converters\CurrencyToString.cs',
    _project.Files[14]);

  AssertEquals('File 16',
    'UI\Converters\DateTimeToDateTimeOffset.cs',
    _project.Files[15]);

  AssertEquals('File 17',
    'UI\Converters\DateToString.cs',
    _project.Files[16]);

  AssertEquals('File 18',
    'UI\Converters\DecimalToString.cs',
    _project.Files[17]);

  AssertEquals('File 19',
    'UI\Converters\TimeToString.cs',
    _project.Files[18]);
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

