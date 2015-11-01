unit FileModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil;

type
  TFileModel = Class(TComponent)
    private
      _path: String;
      _title: String;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure Load(path: String);
    published
      property Path: String read _path write _path;
      property Title: String read _title write _title;
  end;

implementation

constructor TFileModel.Create(aOwner: TComponent);
begin
  inherited;
end;

destructor TFileModel.Destroy;
begin
  inherited;
end;

procedure TFileModel.Load(path: String);
begin
  if FileExistsUTF8(path) then begin
    Title := ExtractFileNameOnly(path);
  end;
end;

end.

