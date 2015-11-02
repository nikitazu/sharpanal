unit FileHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil;

function PathCombine(args: Array of ShortString): String;

implementation

function PathCombine(args: Array of ShortString): String;
var
  i: Integer;
  count: Integer;
begin
  count := Length(args);
  Result := '';
  if count >= 1 then Result := args[0];
  if count > 1 then
    for i:= 1 to count - 1 do
      Result := AppendPathDelim(Result) + args[i];
end;

end.

