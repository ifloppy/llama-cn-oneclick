unit commonunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

const
  PleaseSelectImpl = '选择一个实现方式';
  PleaseSelectModel = '选择一个模型';
  UserAgent = 'llama-cn-oneclick';


procedure LoadConfig();
function isEmptyDirectory(aDir:string):boolean;

var
   config: TIniFile;

implementation



procedure LoadConfig();
begin
  config:=TIniFile.Create('config.ini');

end;

function isEmptyDirectory(aDir:string):boolean;
var
  SearchRecResult: TSearchRec;
begin
  Result := FindFirst(IncludeTrailingPathDelimiter(aDir) + '*', faAnyFile, SearchRecResult)<>0;

  while (SearchRecResult.Name = '.') or (SearchRecResult.Name = '..') do
    Result := FindNext(SearchRecResult) <> 0;;

  FindClose(SearchRecResult);
end;

end.

