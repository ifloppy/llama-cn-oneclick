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

var
   config: TIniFile;

implementation



procedure LoadConfig();
begin
  config:=TIniFile.Create('config.ini');

end;

end.

