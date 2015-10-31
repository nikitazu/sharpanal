unit SolutionModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSolutionModel = Class(TComponent)
    public
      procedure Load(path: String);
  end;

implementation

procedure TSolutionModel.Load(path: String);
begin
  WriteLn('TODO ', path);
end;

end.

