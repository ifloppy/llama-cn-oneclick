unit setup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, fphttpclient,
  fpjson, jsonparser, Zipper, process, Contnrs, StrUtils, FileUtil;

type

  { TFormSetup }

  TFormSetup = class(TForm)
    btnAutoLLAMAImpl: TButton;
    btnInstallModel: TButton;
    btnInstallLLAMA: TButton;
    btnSaveLLAMAProfile: TButton;
    btnAutoLLAMAVersion: TButton;
    btnSaveModelProfile: TButton;
    btnLaunch: TButton;
    btnQuit: TButton;
    inputModel: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    inputImplementation: TComboBox;
    inputVersion: TLabeledEdit;
    SaveScriptFile: TSaveDialog;
    procedure btnAutoLLAMAImplClick(Sender: TObject);
    procedure btnAutoLLAMAVersionClick(Sender: TObject);
    procedure btnInstallLLAMAClick(Sender: TObject);
    procedure btnInstallModelClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure btnSaveLLAMAProfileClick(Sender: TObject);
    procedure btnSaveModelProfileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure inputImplementationChange(Sender: TObject);
  private
    ModelRepo: TFPStringHashTable;
  public

  end;

var
  FormSetup: TFormSetup;

implementation

uses commonunit, launch;

{$R *.lfm}

{ TFormSetup }

procedure TFormSetup.FormCreate(Sender: TObject);
var
  output: string;
  modelLines: TStrings;
  sCur: string;
  SplitedString: array of string;
  i: UInt8;
begin
  if not DirectoryExists('llama') then CreateDir('llama');
  if not DirectoryExists('models') then CreateDir('models');
  try
    RunCommand('git', output)
  except
    MessageDlg('缺少依赖', '你没有安装git，下载模型需要依赖git!', mtWarning, [mbOK], 0)
  end;

  LoadConfig();
  inputVersion.Caption:=config.ReadString('llama', 'version', '');
  inputImplementation.Text:=config.ReadString('llama', 'implementation', PleaseSelectImpl);
  inputModel.Text:=config.ReadString('model', 'name', PleaseSelectModel);

  modelLines:=TStringList.Create;
  try
    modelLines.LoadFromFile('models.txt');
  except
    begin
      MessageDlg('错误', '没有找到模型仓库文件，请确认你有将本程序完整地解压出来再运行！', mtError, [mbOK], 0);
      Halt;
    end;
  end;


  ModelRepo:=TFPStringHashTable.Create;
  for i := 0 to Pred(modelLines.Count) do
  begin
    sCur:=modelLines[i];
    if sCur<>'' then begin
      SplitedString:=SplitString(sCur, '|');
      inputModel.Items.Append(SplitedString[0]);
      ModelRepo.Add(SplitedString[0], SplitedString[1]);
    end;
  end;
  modelLines.Free;
end;

procedure TFormSetup.btnAutoLLAMAImplClick(Sender: TObject);
begin
  inputImplementation.Text:='avx2';
end;

procedure TFormSetup.btnAutoLLAMAVersionClick(Sender: TObject);
var
  client: TFPHTTPClient;
  resp: string;
  json: TJSONObject;
begin
  client:=TFPHTTPClient.Create(nil);
  client.AddHeader('User-Agent', UserAgent);
  client.AddHeader('Accept', 'application/json');
  resp:=client.Get('https://api.github.com/repos/ggerganov/llama.cpp/releases/latest');
  json:=GetJSON(resp) as TJSONObject;
  inputVersion.Caption:=json.Strings['tag_name'];
  json.Free;
  client.Free;
end;

procedure TFormSetup.btnInstallLLAMAClick(Sender: TObject);
{var
  client: TFPHTTPClient;
  resp_tag: string;
  json_tag: TJSONObject;
begin
  client:=TFPHTTPClient.Create(nil);
  client.AddHeader('User-Agent', UserAgent);
  client.AddHeader('Accept', 'application/json');
  resp_tag:=client.Get('https://api.github.com/repos/ggerganov/llama.cpp/releases/tags/'+inputVersion);
  json_tag:=GetJSON(resp_tag) as TJSONObject;

end; }
var
  client: TFPHTTPClient;
  url: string;
  outputfile: TFileStream;
  Unzipper: TUnZipper;
begin
  if (inputVersion.Caption = '') or (inputImplementation.Text = PleaseSelectImpl) then begin
    ShowMessage('请填写正确的参数');
    exit;
  end;
  client:=TFPHTTPClient.Create(nil);
  client.AddHeader('User-Agent', UserAgent);
  client.AllowRedirect:=true;
  DeleteFile('llama.zip');
  outputfile:=TFileStream.Create('llama.zip', fmCreate);
  url:='https://github.com/ggerganov/llama.cpp/releases/download/'+inputVersion.Caption+'/llama-'+inputVersion.Caption+'-bin-win-'+inputImplementation.Text+'-x64.zip';
  client.Get(url, outputfile);
  client.Free;
  outputfile.Free;
  Unzipper:=TUnZipper.Create;
  Unzipper.OutputPath:='llama';
  Unzipper.UnZipAllFiles('llama.zip');
  Unzipper.Free;
  DeleteFile('llama.zip');
  ShowMessage('若刚刚没有出现报错信息，则说明LLaMA.cpp已成功安装');
end;

procedure TFormSetup.btnInstallModelClick(Sender: TObject);
var
  script: TStringStream;
begin
  //ShowMessage(ModelRepo.Items[inputModel.Caption]);
  script:=TStringStream.Create('git lfs install'+LineEnding+'git clone '+ModelRepo.Items[inputModel.Caption]+' '+GetCurrentDir+PathDelim+'models'+LineEnding+'pause');
  if SaveScriptFile.Execute then script.SaveToFile(SaveScriptFile.FileName);


  if not isEmptyDirectory('models') then if MessageDlg('模型目录非空', '安装新的模型需要将models目录清空。点击“是”则会自动清空；“否”则由你稍后自行清空', mtConfirmation, mbYesNo, 0) = mrYes then DeleteDirectory('models', true);
  script.Free;
end;

procedure TFormSetup.btnLaunchClick(Sender: TObject);
begin

  //FormLaunch:=TFormLaunch.Create(nil);
  //FormLaunch.Show;

  Application.CreateForm(TFormLaunch, FormLaunch);

  Close;
end;

procedure TFormSetup.btnQuitClick(Sender: TObject);
begin
  Halt;
end;

procedure TFormSetup.btnSaveLLAMAProfileClick(Sender: TObject);
begin
  if (inputVersion.Caption = '') or (inputImplementation.Text = PleaseSelectImpl) then begin
    ShowMessage('请填写正确的参数');
    exit;
  end;
  config.WriteString('llama', 'version',inputVersion.Caption);
  config.WriteString('llama', 'implementation', inputImplementation.Caption);
end;

procedure TFormSetup.btnSaveModelProfileClick(Sender: TObject);
begin
  if (inputModel.Caption = PleaseSelectModel) then begin
    ShowMessage('请填写正确的参数');
    exit;
  end;
  config.WriteString('model', 'name', inputModel.Caption);
end;

procedure TFormSetup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ModelRepo.Free;

end;

procedure TFormSetup.inputImplementationChange(Sender: TObject);
begin
  if inputImplementation.Caption='avx512' then ShowMessage('注意:许多机器上无法使用AVX512指令集，若在不支持AVX512的机型上选择了该选项，则会出现llama无法启动的情况。如果你不确定自己在做什么，请点击“自动填写”');
end;

end.

