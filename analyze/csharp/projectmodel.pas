unit ProjectModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil;

type
  TProjectModel = Class(TComponent)
    private
     _path: String;
     _title: String;
    public
      procedure Load(path: String);
    published
      property Path: String read _path write _path;
      property Title: String read _title write _title;
  end;

implementation

procedure TProjectModel.Load(path: String);
begin
  if FileExistsUTF8(path) then begin
    Title := ExtractFileNameOnly(path);
  end;
end;

end.

