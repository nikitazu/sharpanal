unit ProjectModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil,
  Laz2_DOM, laz2_XMLRead;

type
  TProjectModel = Class(TComponent)
    private
      _path: String;
      _title: String;
      _files: TStringList;
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
      procedure Load(path: String);
    published
      property Path: String read _path write _path;
      property Title: String read _title write _title;
      property Files: TStringList read _files write _files;
  end;

implementation

constructor TProjectModel.Create(aOwner: TComponent);
begin
  inherited;
  _files := TStringList.Create;
end;

destructor TProjectModel.Destroy;
begin
  FreeAndNil(_files);
  inherited;
end;

procedure TProjectModel.Load(path: String);
var
  document: TXMLDocument;
  itemGroup: TDOMNode;
  item: TDOMNode;
  itemFile: String;
begin
  if FileExistsUTF8(path) then begin
    Title := ExtractFileNameOnly(path);
    try
      ReadXMLFile(document, path);
      itemGroup := document.DocumentElement.FindNode('ItemGroup');
      while Assigned(itemGroup) do begin
        item := itemGroup.FindNode('Compile');
        while Assigned(item) do begin
          itemFile := item.Attributes.GetNamedItem('Include').TextContent;
          _files.Add(itemFile);
          item := item.NextSibling;
        end;
        itemGroup := itemGroup.NextSibling;
      end;
    finally
      FreeAndNil(document);
    end;
  end;
end;

end.

