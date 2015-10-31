unit SolutionModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  strutils, FileUtil;

type
  TSolutionModel = Class(TComponent)
    private
      _title: String;
      _projects: TStringList;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure Load(path: String);
    published
      property Title: String read _title write _title;
      property Projects: TStringList read _projects write _projects;
  end;

implementation

constructor TSolutionModel.Create(aOwner: TComponent);
begin
  inherited;
  _projects := TStringList.Create;
end;

destructor TSolutionModel.Destroy;
begin
  FreeAndNil(_projects);
  inherited;
end;

procedure TSolutionModel.Load(path: String);
var
  f: TextFile;
  currentLine: String;
  currentProjectPath: String;
begin
  if FileExistsUTF8(path) then begin
    Title := ExtractFileNameOnly(path);
    AssignFile(f, path);
    try
      Reset(f);
      while not EOF(f) do begin
        ReadLn(f, currentLine);
        if AnsiStartsStr('Project', currentLine) then begin
           currentProjectPath := ExtractDelimited(6, currentLine, ['"']);
           Projects.Add(currentProjectPath);
        end;
      end;
    finally
      CloseFile(f);
    end;
  end;
end;

end.

