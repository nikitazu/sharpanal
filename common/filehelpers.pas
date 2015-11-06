unit FileHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FileUtil;

function PathCombine(args: Array of ShortString): String;
function NameToIndex(name: String): String;

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

function NameToIndex(name: String): String;
var
  i: Integer;
  c: Char;
  cs: Array of Char;
begin
  i := 0;
  SetLength(cs, Length(name));
  for c in name do begin
    if c = UpCase(c) then begin
      cs[i] := LowerCase(c);
      Inc(i);
    end;
  end;
  SetLength(cs, i + 1);
  Result := String(cs);
end;

end.

