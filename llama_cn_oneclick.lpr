program llama_cn_oneclick;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, setup, commonunit
  { you can add units after this }, opensslsockets, launch;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  FormSetup:=TFormSetup.Create(nil);
  FormSetup.ShowModal;
  FormSetup.Free;


  Application.Run;
  config.Free;
end.

