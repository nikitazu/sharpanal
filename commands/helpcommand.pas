unit HelpCommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  AbstractCommand;

type
  THelpCommand = class(TAbstractCommand)
    public
      class function CommandName: ShortString;
      procedure Run; override;
  end;

implementation

class function THelpCommand.CommandName: ShortString;
begin
  Result := 'help';
end;

procedure THelpCommand.Run;
var
  filename: String;
begin
  inherited;
  filename := ExtractFileNameOnly(_app.ExeName);
  writeln('Usage: ',filename,' -h');
  writeln(filename,' config --key foo');
  writeln(filename,' init --name db');
  writeln(filename,' link --name db --path vs/projects/solution.sln');
  writeln(filename,' update --name db');
end;

end.

