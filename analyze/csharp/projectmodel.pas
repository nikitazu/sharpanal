unit ProjectModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TProjectModel = Class(TComponent)
    private
     _path: String;
    public
      procedure Load(path: String);
    published
      property Path: String read _path write _path;
  end;

implementation

procedure TProjectModel.Load(path: String);
begin
  WriteLn('TODO ', path);
end;

end.

