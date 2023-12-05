unit launch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, FileUtil, process, LCLIntf;

type

  { TFormLaunch }

  TFormLaunch = class(TForm)
    btnLaunch: TButton;
    btnSave: TButton;
    btnVisitWebServer: TButton;
    inputWebListen: TEdit;
    GroupBox3: TGroupBox;
    ShowCommandline: TEdit;
    inputModelFile: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    inputPrompt: TMemo;
    Process1: TProcess;
    rbConsole: TRadioButton;
    rbWebServer: TRadioButton;
    RadioGroup1: TRadioGroup;
    inputWebPort: TSpinEdit;
    procedure btnSaveClick(Sender: TObject);
    procedure btnVisitWebServerClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormLaunch: TFormLaunch;

implementation

uses commonunit;

  {$R *.lfm}

  { TFormLaunch }

procedure TFormLaunch.btnLaunchClick(Sender: TObject);
begin
  if Process1.Running then
  begin
    ShowMessage('已经有一个实例正在运行，暂不支持同时运行多个实例');
    exit;
  end;

  //This is a args generator
  Process1.Parameters.Clear;



  //shared args
  //model
  Process1.Parameters.Add('-m');
  Process1.Parameters.Add(GetCurrentDir + '\models\' + inputModelFile.Caption);



  if rbConsole.Checked then
  begin
    //Use console mode
    Process1.Parameters.Add('--color');
    Process1.Parameters.Add('-i');

    //prompt
    Process1.Parameters.Add('-p');
    Process1.Parameters.Add(QuotedStr(inputPrompt.Text));

    Process1.Executable := GetCurrentDir + '\llama\main.exe';
  end;

  if rbWebServer.Checked then
  begin
    Process1.Parameters.Add('--port');
    Process1.Parameters.Add(inputWebPort.Text);
    Process1.Parameters.Add('--host');
    Process1.Parameters.Add(inputWebListen.Text);


    Process1.Executable := GetCurrentDir + '\llama\server.exe';
  end;

  Process1.Execute;
  Process1.Parameters.StrictDelimiter := True;
  Process1.Parameters.Delimiter := ' ';
  ShowCommandline.Text := Process1.Executable + ' ' + Process1.Parameters.DelimitedText;
end;

procedure TFormLaunch.btnSaveClick(Sender: TObject);
begin
  config.WriteString('launch', 'prompt', inputPrompt.Text);
  config.WriteString('launch', 'model', inputModelFile.Text);
  config.WriteString('launch', 'web_host', inputWebListen.Text);
  config.WriteString('launch', 'web_port', inputWebPort.Text);
end;

procedure TFormLaunch.btnVisitWebServerClick(Sender: TObject);
begin
  OpenURL('http://' + inputWebListen.Text + ':' + inputWebPort.Text);
end;

procedure TFormLaunch.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TFormLaunch.FormCreate(Sender: TObject);
var
  F: TSearchRec;
  s: string;
begin
  inputPrompt.Lines.Text := config.ReadString('launch', 'prompt', defaultPrompt);
  inputModelFile.Text := config.ReadString('launch', 'model', '');
  inputWebListen.Text := config.ReadString('launch', 'web_host', '127.0.0.1');
  inputWebPort.Text := config.ReadString('launch', 'web_port', '8080');

  if FindFirst(GetCurrentDir + '\models\*.*', faAnyFile, F) = 0 then
  begin
    repeat
      with F do
      begin
        s := ExtractFileName(F.Name);
        case s of
          //Without common files that are not model files
          'README.md': Break;
          '.gitignore': Break;
          '.gitattributes': Break;
          '.': Break;
          '..': Break;
          '.git': Break;
        end;
        inputModelFile.Items.Add(s);
      end;
    until FindNext(F) <> 0;
  end;
  FindClose(F);

end;

end.
