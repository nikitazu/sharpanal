unit Entities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  SolutionIdentity = Type Integer;
  ProjectIdentity = Type Integer;
  FileIdentity = Type Integer;

  SolutionEntity = record
    Id: SolutionIdentity;
    Title: String;
    TitleIndex: String;
    Path: String;
  end;

  ProjectEntity = record
    Id: ProjectIdentity;
    Title: String;
    TitleIndex: String;
    Path: String;
    SolutionId: SolutionIdentity;
  end;

  FileEntity = record
    Id: FileIdentity;
    Title: String;
    TitleIndex: String;
    Path: String;
    SolutionId: SolutionIdentity;
    ProjectId: ProjectIdentity;
  end;

implementation

end.

