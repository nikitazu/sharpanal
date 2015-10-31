unit FileModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFileModel = Class(TComponent)
    private
     _path: String;
    published
      property Path: String read _path write _path;
  end;

implementation

end.

